(*
    Copyright 2018 Fernando Borretti <fernando@borretti.me>

    This file is part of L0.

    L0 is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    L0 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with L0.  If not, see <http://www.gnu.org/licenses/>.
*)

structure TAST :> TAST = struct
  datatype tast = TConstUnit
                | TConstBool of bool
                | TConstInt of int * Type.ty
                | TConstString of string
                | TVar of Ident.ident * Type.ty
                | TBinop of Binop.binop * tast * tast * Type.ty
                | TCond of tast * tast * tast * Type.ty
                | TCast of Type.ty * tast
                | TProgn of tast list
                | TLet of Ident.ident * tast * tast
                | TAssign of Ident.ident * tast
                | TNullPtr of Type.ty
                | TLoad of tast * Type.ty
                | TStore of tast * tast
                | TMalloc of Type.ty * tast
                | TFree of tast
                | TAddressOf of Ident.ident * Type.ty
                | TPrint of tast
                | TCEmbed of Type.ty * string
                | TCCall of string * Type.ty * tast list
                | TWhile of tast * tast
                | TFuncall of string * tast list * Type.ty

  local
    open Type
    open Function
  in
    val defaultIntType = Int (Signed, Word32)

    fun typeOf TConstUnit = Unit
      | typeOf (TConstBool _) = Bool
      | typeOf (TConstInt (_, t)) = t
      | typeOf (TConstString _) = Str
      | typeOf (TVar (_, t)) = t
      | typeOf (TBinop (_, _, _, t)) = t
      | typeOf (TCond (_, _, _, t)) = t
      | typeOf (TCast (t, _)) = t
      | typeOf (TProgn ls) =
        if (length ls = 0) then
            Unit
        else
            typeOf (List.last ls)
      | typeOf (TLet (_, _, b)) = typeOf b
      | typeOf (TAssign (_, v)) = typeOf v
      | typeOf (TNullPtr t) = RawPointer t
      | typeOf (TLoad (_, t)) = t
      | typeOf (TStore (_, v)) = typeOf v
      | typeOf (TMalloc (t, _)) = RawPointer t
      | typeOf (TFree _) = Unit
      | typeOf (TAddressOf (_, t)) = RawPointer t
      | typeOf (TPrint _) = Unit
      | typeOf (TCEmbed (t, _)) = t
      | typeOf (TCCall (_, t, _)) = t
      | typeOf (TWhile _) = Unit
      | typeOf (TFuncall (_, _, t)) = t

    datatype context = Context of Function.stack * Type.tenv * Function.fenv

    fun mkContext s t f = Context (s, t, f)

    fun ctxStack (Context (s, _, _)) = s
    fun ctxTenv (Context (_, t, _)) = t
    fun ctxFenv (Context (_, _, f)) = f

    local
      open OAST
      open Binop
    in
      fun augment ConstUnit _ = TConstUnit
        | augment (ConstBool b) _ = TConstBool b
        | augment (ConstInt i) _ = TConstInt (i, defaultIntType)
        | augment (ConstString s) _ = TConstString s
        | augment (Var i) c =
          (case (Map.get (ctxStack c) i) of
               SOME bind => TVar (i, bindType bind)
             | NONE => raise Fail "No such variable")
        | augment (Binop (Add, a, b)) c = augmentArithOp Add a b c
        | augment (Binop (Sub, a, b)) c = augmentArithOp Sub a b c
        | augment (Binop (Mul, a, b)) c = augmentArithOp Mul a b c
        | augment (Binop (Div, a, b)) c = augmentArithOp Div a b c
        | augment (Binop (Eq, a, b)) c = augmentCompOp Eq a b c
        | augment (Binop (NEq, a, b)) c = augmentCompOp NEq a b c
        | augment (Binop (LT, a, b)) c = augmentCompOp LT a b c
        | augment (Binop (LEq, a, b)) c = augmentCompOp LEq a b c
        | augment (Binop (GT, a, b)) c = augmentCompOp GT a b c
        | augment (Binop (GEq, a, b)) c = augmentCompOp GEq a b c
        | augment (Cast (ty, a)) c =
          let val a' = augment a c
          in
              if (Type.isNumeric ty) then
                  if (Type.isNumeric (typeOf a')) then
                      TCast (ty, a')
                  else
                      raise Fail "Cannot cast this type to a numeric type"
              else
                  raise Fail "Casting to this type is not supported"
          end
        | augment (Cond (test, cons, alt)) c =
          let val test' = augment test c
              and cons' = augment cons c
              and alt' = augment alt c
          in
              if (typeOf test') <> Bool then
                  raise Fail "The test in an if must be of boolean type"
              else
                  if (typeOf cons') <> (typeOf alt') then
                      raise Fail "The consequent and the alternate must have the same type"
                  else
                      TCond (test', cons', alt', typeOf cons')
          end
        | augment (Progn exps) c =
          TProgn (map (fn a => augment a c) exps)
        | augment (Let (name, v, body)) c =
          let val v' = augment v c
          in
              let val s' = Map.iadd (ctxStack c)
                                    (name, (Binding (typeOf v', Mutable)))
              in
                  TLet (name,
                        v',
                        augment body (mkContext s' (ctxTenv c) (ctxFenv c)))
              end
          end
        | augment (Assign (var, v)) c =
          let val v' = augment v c
          in
              let val (Binding (ty, m)) = case (Map.get (ctxStack c) var) of
                                              SOME bind => bind
                                            | NONE => raise Fail "No such variable"
              in
                  if m = Mutable then
                      if typeOf v' = ty then
                          TAssign (var, v')
                      else
                          raise Fail ("Cannot assign to variable '"
                                      ^ (Int.toString (Ident.identName var))
                                      ^ "': the type of the variable is "
                                      ^ (tyToString ty)
                                      ^ ", while the type of the expression is "
                                      ^ (tyToString (typeOf v')))
                  else
                      raise Fail ("Cannot assign to immutable variable '"
                                  ^ (Int.toString (Ident.identName var))
                                  ^ "'")
              end
          end
        | augment (NullPtr t) c =
          TNullPtr (parseTypeSpecifier t (ctxTenv c))
        | augment (Load e) c =
          let val e' = augment e c
          in
              case (typeOf e') of
                  RawPointer t => TLoad (e', t)
                | _ => raise Fail "load: not a pointer"
          end
        | augment (Store (p, v)) c =
          let val p' = augment p c
              and v' = augment v c
          in
              let fun mkStore t = let val ty = typeOf v'
                                  in
                                      if ty = t then
                                          TStore (p', v')
                                      else
                                          raise Fail "store: type mismatch"
                                  end
              in
                  case (typeOf p') of
                      RawPointer t => mkStore t
                    | _ => raise Fail "store: first argument must be a pointer"
              end
          end
        | augment (Malloc (ty, s)) c =
          let val t' = parseTypeSpecifier ty (ctxTenv c)
              and s' = augment s c
          in
              if (typeOf s' <> Int (Unsigned, Word64)) then
                  raise Fail "malloc: allocation count must be u64"
              else
                  TMalloc (t', s')
          end
        | augment (Free p) c =
          let val p' = augment p c
          in
              case (typeOf p') of
                  (RawPointer _) => TFree p'
                | _ => raise Fail "Can't free a non-pointer"
          end
        | augment (AddressOf n) c =
          let val (Binding (ty, _)) = case Map.get (ctxStack c) n of
                                          SOME b => b
                                        | NONE => raise Fail "Cannot find variable"
          in
              TAddressOf (n, ty)
          end
        | augment (Print v) c =
          let val v' = augment v c
          in
              if isPrintable (typeOf v') then
                  TPrint v'
              else
                  raise Fail "Type cannot be printed"
          end
        | augment (CEmbed (ts, code)) c =
          TCEmbed (parseTypeSpecifier ts (ctxTenv c), code)
        | augment (CCall (n, ts, args)) c =
          let val ty = parseTypeSpecifier ts (ctxTenv c)
              and args = map (fn a => augment a c) args
          in
              TCCall (n, ty, args)
          end
        | augment (While (test, body)) c =
          let val test' = augment test c
              and body' = augment body c
          in
              if typeOf test' <> Bool then
                  raise Fail "The test of a while loop must be a boolean expression"
              else
                  TWhile (test', body')
          end
        | augment (Funcall (name, args)) c =
          (case Map.get (ctxFenv c) name of
               SOME (Function (_, params, rt)) =>
               let val targs = (map (fn e => augment e c) args)
               in
                   if (Function.matchParams params (map typeOf targs)) then
                       TFuncall (name, targs, rt)
                   else
                       raise Fail "Argument types don't match"
               end
             | NONE => raise Fail "No such function")
      and augmentArithOp oper a b ctx =
          let val a' = augment a ctx
              and b' = augment b ctx
          in
              if (typeOf a') <> (typeOf b') then
                  raise Fail "Both operands to an arithmetic operation must be of the same type"
              else
                  let val t = typeOf a'
                  in
                      if (isNumeric t) then
                          TBinop (oper, a', b', t)
                      else
                          raise Fail "Can't perform arithmetic on non-numeric types"
                  end
          end
      and augmentCompOp oper a b ctx =
          let val a' = augment a ctx
              and b' = augment b ctx
          in
              let val ta = typeOf a'
                  and tb = typeOf b'
              in
                  if ta <> tb then
                      raise Fail ("Both operands to "
                                  ^ (Binop.binopName oper)
                                  ^ " must be of the same type. Types: "
                                  ^ (tyToString ta)
                                  ^ " and "
                                  ^ (tyToString tb))
                  else
                      if isEquatable ta then
                          TBinop (oper, a', b', Bool)
                      else
                          raise Fail ("Cannot compare objects of this type: " ^ (tyToString ta))
              end
          end
    end
  end
end
