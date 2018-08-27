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

structure CppBackend :> CPP_BACKEND = struct
  (* Prelude *)

  val prelude =
      String.concatWith "\n" ["#include <cstdlib>",
                              "#include <cstdio>",
                              "#include <cinttypes>",
                              "#include <tuple>"]

  (* Context *)

  datatype context = Context of CppAst.top_ast list

  val emptyContext = Context []

  fun renderContext (Context ts) =
    prelude ^ "\n\n" ^ (String.concatWith "\n\n" (map CppAst.renderTop ts))

  fun ctxToplevel (Context t) = t

  (* Fresh identifiers *)

  val count = ref 0
  fun freshVar () =
    let
    in
        count := !count + 1;
        "autovar_" ^ (Int.toString (!count))
    end

  (* Variables *)

  fun varName n =
    "var_" ^ (Ident.identName n)

  fun ngVar n =
    CppAst.Var (varName n)

  (* Mapping types *)

  local
    open Type
  in
    fun convertIntType Unsigned Word8 =  CppAst.UInt8
      | convertIntType Signed   Word8 =  CppAst.Int8
      | convertIntType Unsigned Word16 = CppAst.UInt16
      | convertIntType Signed   Word16 = CppAst.Int16
      | convertIntType Unsigned Word32 = CppAst.UInt32
      | convertIntType Signed   Word32 = CppAst.Int32
      | convertIntType Unsigned Word64 = CppAst.UInt64
      | convertIntType Signed   Word64 = CppAst.Int64
  end

  local
    open CppAst
  in
    fun convertType (Type.Unit) _ = Bool
      | convertType (Type.Bool) _ = Bool
      | convertType (Type.Int (s, w)) _ = convertIntType s w
      | convertType (Type.Str) _ = Pointer UInt8
      | convertType (Type.RawPointer t) ctx = Pointer (convertType t ctx)
      | convertType (Type.Tuple ts) ctx = Tuple (map (fn t => convertType t ctx) ts)
  end

  (* Printing *)

  local
    open Type
    open CppAst
  in
    fun formatStringFor Unit n = [ConstString ("nil" ^ (newline n))]
      | formatStringFor Type.Bool _ = raise Fail "bool can't be printf'd"
      | formatStringFor (Int (Unsigned, Word8)) n = wrap "PRIu8" n
      | formatStringFor (Int (Signed,   Word8)) n = wrap "PRIi8" n
      | formatStringFor (Int (Unsigned, Word16)) n = wrap "PRIu16" n
      | formatStringFor (Int (Signed,   Word16)) n = wrap "PRIi16" n
      | formatStringFor (Int (Unsigned, Word32)) n = wrap "PRIu32" n
      | formatStringFor (Int (Signed,   Word32)) n = wrap "PRIi32" n
      | formatStringFor (Int (Unsigned, Word64)) n = wrap "PRIu64" n
      | formatStringFor (Int (Signed,   Word64)) n = wrap "PRIi64" n
      | formatStringFor Str n = [ConstString ("%s" ^ (newline n))]
      | formatStringFor (RawPointer _) n = [ConstString ("%p" ^ (newline n))]
      | formatStringFor _ _ = raise Fail "Records cannot be printf'd"
    and wrap s n = [Adjacent [ConstString "%", Var s, ConstString (newline n)]]
    and newline OAST.Newline = "\\n"
      | newline OAST.NoNewline = ""
  end

  (* TAST -> T CAST *)

  local
    open TAST
    open CppAst
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
      | convert (TTuple exps) ctx =
        let val args = map (fn e => convert e ctx) exps
        in
            (Sequence (map (fn (b, _) => b) args),
             CreateTuple (map (fn (_, e) => e) args))
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
      | convert (TPrint (v, n)) ctx =
        let val (vblock, vval) = convert v ctx
            and ty = typeOf v
        in
            let val printer = if ty = Type.Bool then
                                  let val nl = (case n of
                                                    OAST.Newline => ConstBool true
                                                  | OAST.NoNewline => ConstBool false)
                                  in
                                      Funcall (NONE, "interim_print_bool", [vval, nl])
                                  end
                              else
                                  Funcall (NONE, "printf", (formatStringFor ty n) @ [vval])
            in
                (Sequence [vblock, printer],
                 unitConstant)
            end
        end
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

    fun defineFunction ctx (Function.Function (name, params, rt)) tast =
      let val (block, retval) = convert tast ctx
      in
          let val def = FunctionDef (name,
                                     map (convertParam ctx) params,
                                     convertType rt ctx,
                                     block,
                                     retval)
          in
              Context (ctxToplevel ctx @ [def])
          end
      end
    and convertParam ctx (Function.Param (i, t)) =
      Param ("var_" ^ (Ident.identName i), convertType t ctx)

    fun defineStruct name slots ctx =
      StructDef (name, map (fn (Type.Slot (n, t)) => Slot (n, convertType t ctx)) slots)
  end
end
