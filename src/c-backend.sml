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

structure CBackend :> C_BACKEND = struct
  type tuple_types = Type.ty OrderedSet.set

  datatype context = Context of CAst.top_ast list * tuple_types

  val emptyContext = Context ([], OrderedSet.empty)

  fun renderContext (Context (ts, _)) =
    String.concatWith "\n\n" (map CAst.renderTop ts)

  fun ctxTupleTypes (Context (_, tt)) = tt

  (* Extract tuple types from TAST expressions *)

  local
    open TAST
    open OrderedSet
  in
    fun allTypes (TBinop (_, lhs, rhs, ty)) = unionList [allTypes lhs,
                                                         allTypes rhs,
                                                         singleton ty]
      | allTypes (TCond (t, c, a, ty)) = unionList [allTypes t,
                                                    allTypes c,
                                                    allTypes a,
                                                    singleton ty]
      | allTypes (TCast (ty, exp)) = add (allTypes exp) ty
      | allTypes (TProgn l) = unionList (map allTypes l)
      | allTypes (TLet (_, v, exp)) = union (allTypes v) (allTypes exp)
      | allTypes (TAssign (_, exp)) = allTypes exp
      | allTypes (TNullPtr ty) = singleton ty
      | allTypes (TLoad (exp, ty)) = add (allTypes exp) ty
      | allTypes (TStore (ptr, exp)) = union (allTypes ptr) (allTypes exp)
      | allTypes (TMalloc (ty, exp)) = add (allTypes exp) ty
      | allTypes (TFree exp) = allTypes exp
      | allTypes (TPrint exp) = allTypes exp
      | allTypes (TCEmbed (ty, _)) = singleton ty
      | allTypes (TCCall (_, ty, args)) = add (unionList (map allTypes args)) ty
      | allTypes (TWhile (test, body)) = union (allTypes test) (allTypes body)
      | allTypes (TFuncall (_, args, ty)) = add (unionList (map allTypes args)) ty
      | allTypes exp = add empty (typeOf exp)
  end

  local
    open Type
  in
    fun filterTuples types =
        OrderedSet.filter types
                          (fn ty => case ty of
                                        (Tuple _) => true
                                      | _ => false)
  end

  (* Given the ordered set of tuples from a Context, and a TAST node, extract
     all tuple types from that node and add them to the set of tuples. *)
  fun collectTupleTypes tuple_types tast =
    OrderedSet.union tuple_types (filterTuples (allTypes tast))

  (* Fresh identifiers *)

  val count = ref 0
  fun freshVar () =
    let
    in
        count := !count + 1;
        "autovar_" ^ (Int.toString (!count))
    end

  (* TAST -> C AST *)

  fun varName n =
    "var_" ^ (Int.toString (NameGen.nameId n))

  fun ngVar n =
    CAst.Var (varName n)

  local
    open Type
  in
    fun convertIntType Unsigned Word8 =  CAst.UInt8
      | convertIntType Signed   Word8 =  CAst.Int8
      | convertIntType Unsigned Word16 = CAst.UInt16
      | convertIntType Signed   Word16 = CAst.Int16
      | convertIntType Unsigned Word32 = CAst.UInt32
      | convertIntType Signed   Word32 = CAst.Int32
      | convertIntType Unsigned Word64 = CAst.UInt64
      | convertIntType Signed   Word64 = CAst.Int64
  end

  local
    open CAst
  in
    fun convertType (Type.Unit) _ = Bool
      | convertType (Type.Bool) _ = Bool
      | convertType (Type.Int (s, w)) _ = convertIntType s w
      | convertType (Type.Str) _ = Pointer UInt8
      | convertType (Type.RawPointer t) ctx = Pointer (convertType t ctx)
      | convertType (Type.Tuple ts) ctx =
        case OrderedSet.positionOf (ctxTupleTypes ctx) (Type.Tuple ts) of
            SOME i => Struct ("struct_" ^ (Int.toString i))
          | NONE => raise Fail "Tuple not in table"
  end

  local
    open TAST
    open CAst
  in
    val unitConstant = ConstBool false

    fun convert TConstUnit _ =
        (Sequence [], unitConstant)
      | convert (TConstBool b) _ =
        (Sequence [], ConstBool b)
      | convert (TConstInt (i, t)) ctx =
        (Sequence [], Cast (convertType t ctx, ConstInt i))
      | convert (TConstString s) _ =
        (Sequence [], ConstString s)
      | convert (TVar (s, t)) _ =
        (Sequence [], ngVar s)
      | convert (TBinop (oper, a, b, t)) ctx =
        let val (ablock, aval) = convert a ctx
            and (bblock, bval) = convert b ctx
        in
            (Sequence [
                  ablock,
                  bblock
              ],
             Binop (oper, aval, bval))
        end
      | convert (TCond (t, c, a, _)) ctx =
        let val (tblock, tval) = convert t ctx
            and (cblock, cval) = convert c ctx
            and (ablock, aval) = convert a ctx
            and result = freshVar ()
            and resType = convertType (TAST.typeOf c) ctx
        in
            (Sequence [
                  tblock,
                  Declare (resType, result),
                  Cond (tval,
                        Block [
                            cblock,
                            Assign (Var result, cval)
                        ],
                        Block [
                            ablock,
                            Assign (Var result, aval)
                       ])
              ],
             Var result)
        end
      | convert (TCast (ty, a)) ctx =
        let val (ablock, aval) = convert a ctx
        in
            (ablock, Cast (convertType ty ctx, aval))
        end
      | convert (TProgn exps) ctx =
        let val exps' = map (fn e => convert e ctx) exps
        in
            if (length exps = 0) then
                (Sequence [], unitConstant)
            else
                (Sequence (map (fn (b, _) => b) exps'),
                 let val (_, v) = List.last exps' in v end)
        end
      | convert (TLet (name, v, b)) ctx =
        let val (vblock, vval) = convert v ctx
            and ty = convertType (typeOf v) ctx
            and (bblock, bval) = convert b ctx
        in
            (Sequence [vblock, Declare (ty, varName name), Assign (ngVar name, vval), bblock],
             bval)
        end
      | convert (TAssign (var, v)) ctx =
        let val (vblock, vval) = convert v ctx
        in
            (Sequence [vblock, Assign (ngVar var, vval)], vval)
        end
      | convert (TNullPtr _) ctx =
        (Sequence [], ConstNull)
      | convert (TLoad (e, _)) ctx =
        let val (eblock, eval) = convert e ctx
        in
            (eblock, Deref eval)
        end
      | convert (TStore (p, v)) ctx =
        let val (pblock, pval) = convert p ctx
            and (vblock, vval) = convert v ctx
        in
            (Sequence [pblock, vblock, Assign ((Deref pval), vval)], vval)
        end
      | convert (TMalloc (t, c)) ctx =
        let val (cblock, cval) = convert c ctx
            and ty = convertType t ctx
            and res = freshVar ()
        in
            let val sizecalc = Binop (Binop.Mul, cval, SizeOf ty)
            in
                (Sequence [cblock, Declare (Pointer ty, res), Funcall (SOME res, "malloc", [sizecalc])],
                 Cast (Pointer ty, Var res))
            end
        end
      | convert (TFree p) ctx =
        let val (pblock, pval) = convert p ctx
        in
            (Sequence [pblock, Funcall (NONE, "free", [pval])], unitConstant)
        end
      | convert (TAddressOf (v, _)) _ =
        (Sequence [], AddressOf (ngVar v))
      | convert (TPrint _) _ =
        raise Fail "print not implemented yet"
      | convert (TCEmbed (t, s)) ctx =
        (Sequence [], Cast (convertType t ctx, Raw s))
      | convert (TCCall (f, t, args)) ctx =
        let val args' = map (fn a => convert a ctx) args
            and t' = convertType t ctx
        in
             let val blocks = map (fn (b, _) => b) args'
                 and argvals = map (fn (_, v) => v) args'
             in
                 if t = Type.Unit then
                     (Sequence (blocks @ [Funcall (NONE, f, argvals)]),
                      unitConstant)
                 else
                     let val res = freshVar ()
                     in
                         (Sequence (blocks @ [Declare (t', res), Funcall (SOME res, f, argvals)]),
                          Var res)
                     end
             end
        end
      | convert (TWhile (t, b)) ctx =
        let val (tblock, tval) = convert t ctx
            and (bblock, _) = convert b ctx
        in
            (Sequence [tblock, While (tval, bblock)], unitConstant)
        end
      | convert (TFuncall (f, args, rt)) ctx =
        let val args' = map (fn a => convert a ctx) args
            and rt' = convertType rt ctx
            and res = freshVar ()
        in
            let val blocks = map (fn (b, _) => b) args'
                and argvals = map (fn (_, v) => v) args'
            in
                (Sequence (blocks @ [Declare (rt', res), Funcall (SOME res, f, argvals)]),
                 Var res)
            end
        end

    fun defineFunction (Function.Function (name, params, rt)) tast ctx =
      let val (block, retval) = convert tast ctx
      in
          FunctionDef (name,
                       map (fn (Function.Param (n,t)) => Param (n, convertType t ctx)) params,
                       convertType rt ctx,
                       block,
                       retval)
      end

    fun defineStruct name slots ctx =
      StructDef (name, map (fn (Type.Slot (n, t)) => Slot (n, convertType t ctx)) slots)
  end
end
