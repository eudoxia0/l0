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
  type tuple_types = CAst.ty OrderedSet.set

  datatype context = Context of CAst.top_ast list * tuple_types

  val emptyContext = Context ([], OrderedSet.empty)

  fun renderContext (Context (ts, _)) =
      String.concatWith "\n\n" (map CAst.renderTop ts)

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

  fun ngVar n =
    Var ("var_" ^ (Int.toString (NameGen.nameId n)))

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

  fun convertType (Type.Unit) = Bool
    | convertType (Type.Bool) = Bool
    | convertType (Type.Int (s, w)) = convertIntType s w
    | convertType (Type.Str) = Pointer UInt8
    | convertType (Type.RawPointer t) = Pointer (convertType t)
    | convertType (Type.Record (n, _)) = Struct (escapeIdent n)
    | convertType (Type.RegionType _) = RegionType
    | convertType (Type.RegionPointer (t, _)) = Pointer (convertType t)
    | convertType (Type.NullablePointer (t, _)) = Pointer (convertType t)

  local
    open TAST
    open CAst
  in
    fun convert TConstUnit =
        (Sequence [], unitConstant)
      | convert (TConstBool b) =
        (Sequence [], ConstBool b)
      | convert (TConstInt (i, t)) =
        (Sequence [], Cast (convertType t, CConstInt i))
      | convert (TConstString s) =
        (Sequence [], ConstString s)
      | convert (TVar (s, t)) =
        (Sequence [], Var s)
      | convert (TBinop (oper, a, b, t)) =
        let val (ablock, aval) = convert a
            and (bblock, bval) = convert b
        in
            (Sequence [
                  ablock,
                  bblock
              ],
             Binop (oper, aval, bval))
        end
      | convert (TCond (t, c, a, _)) =
        let val (tblock, tval) = convert t
            and (cblock, cval) = convert c
            and (ablock, aval) = convert a
            and result = freshVar ()
            and resType = convertType (TAST.typeOf c)
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
      | convert (TCast (ty, a)) =
        let val (ablock, aval) = convert a
        in
            (ablock, Cast (convertType ty, aval))
        end
      | convert (TProgn exps) =
        let val exps' = map convert exps
        in
            if (length exps = 0) then
                (Sequence [], unitConstant)
            else
                (Sequence (map (fn (b, _) => b) exps'),
                 let val (_, v) = List.last exps' in v end)
        end
      | convert (TLet (name, v, b)) =
        let val (vblock, vval) = convert v
            and ty = convertType (typeOf v)
            and (bblock, bval) = convert b
        in
            (Sequence [vblock, Declare (ty, name), Assign (Var name, vval), bblock],
             bval)
        end
      | convert (TAssign (var, v)) =
        let val (vblock, vval) = convert v
        in
            (Sequence [vblock, Assign (Var var, vval)], vval)
        end
      | convert (TNullPtr _) = (Sequence [], CConstNull)
      | convert (TLoad (e, _)) =
        let val (eblock, eval) = convert e
        in
            (eblock, CDeref eval)
        end
      | convert (TStore (p, v)) =
        let val (pblock, pval) = convert p
            and (vblock, vval) = convert v
        in
            (Sequence [pblock, vblock, Assign ((CDeref pval), vval)], vval)
        end
      | convert (TMalloc (t, c)) =
        let val (cblock, cval) = convert c
            and ty = convertType t
            and res = freshVar ()
        in
            let val sizecalc = CBinop (AST.Mul, cval, CSizeOf ty)
            in
                (Sequence [cblock, Declare (Pointer ty, res), Funcall (SOME res, "malloc", [sizecalc])],
                 Cast (Pointer ty, Var res))
            end
        end
      | convert (TFree p) =
        let val (pblock, pval) = convert p
        in
            (Sequence [pblock, Funcall (NONE, "free", [pval])], unitConstant)
        end
      | convert (TAddressOf (v, _)) =
        (Sequence [], CAddressOf (Var v))
      | convert (TPrint (v, n)) =
        let val (vblock, vval) = convert v
            and ty = typeOf v
        in
            let val printer = if ty = Type.Bool then
                                  let val nl = (case n of
                                                   AST.Newline => CConstBool true
                                                 | AST.NoNewline => CConstBool false)
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
      | convert (TCEmbed (t, s)) =
        (Sequence [], Cast (convertType t, Raw s))
      | convert (TCCall (f, t, args)) =
        let val args' = map (fn a => convert a) args
            and t' = convertType t
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
      | convert (TWhile (t, b)) =
        let val (tblock, tval) = convert t
            and (bblock, _) = convert b
        in
            (Sequence [tblock, CWhile (tval, bblock)], unitConstant)
        end
      | convert (TLetRegion (r, b)) =
        let val (bblock, bval) = convert b
        in
            let val name = regionName r
            in
                (Sequence [Declare (RegionType, name),
                       Funcall (NONE, "interim_region_create", [CAddressOf (Var name)]),
                       bblock,
                       Funcall (NONE, "interim_region_free", [CAddressOf (Var name)])],
                 bval)
            end
        end
      | convert (TAllocate (r, v)) =
        let val (vblock, vval) = convert v
            and cr = CAddressOf (Var (regionName r))
            and res = freshVar ()
            and cty = Pointer (convertType (typeOf v))
        in
            (Sequence [vblock,
                   Declare (cty, res),
                   Funcall (SOME res, "interim_region_allocate", [cr, CSizeOf cty]),
                   Assign (CDeref (Var res), vval)],
             Var res)
        end
      | convert (TNullableCase (p, var, nnc, nc, t)) =
        let val (pblock, pval) = convert p
            and (nncblock, nncval) = convert nnc
            and (ncblock, ncval) = convert nc
            and result = freshVar ()
            and resType = convertType t
        in
            (Sequence [
                  pblock,
                  Declare (resType, result),
                  CCond (CBinop (AST.NEq, pval, CConstNull),
                         Block [
                             Declare (convertType (typeOf p), escapeIdent var),
                             Assign (Var var, pval),
                             nncblock,
                             Assign (Var result, nncval)
                         ],
                         Block [
                             ncblock,
                             Assign (Var result, ncval)
                        ])
              ],
             Var result)
        end
      | convert (TMakeRecord (ty, name, slots)) =
        let val args = map (fn (_, e) => convert e) slots
            and slot_names = map (fn (n, _) => n) slots
        in
            (Sequence (map (fn (b, _) => b) args),
             StructInitializer (name,
                                (ListPair.map (fn (name, v) => (name, v))
                                              (slot_names,
                                               map (fn (_, v) => v) args))))
        end
      | convert (TSlotAccess (r, s, _)) =
        let val (rblock, rval) = convert r
        in
            (rblock, StructAccess (rval, s))
        end
      | convert (TFuncall (f, args, rt)) =
        let val args' = map (fn a => convert a) args
            and rt' = convertType rt
            and res = freshVar ()
        in
            let val blocks = map (fn (b, _) => b) args'
                and argvals = map (fn (_, v) => v) args'
            in
                (Sequence (blocks @ [Declare (rt', res), Funcall (SOME res, f, argvals)]),
                 Var res)
            end
        end

    fun defineFunction (Function.Function (name, params, rt)) tast =
      let val (block, retval) = convert tast
      in
          FunctionDef (name,
                       map (fn (Function.Param (n,t)) => CParam (n, convertParamType t)) params,
                       convertType rt,
                       block,
                       retval)
      end

    fun defineStruct name slots =
      StructDef (name, map (fn (Type.Slot (n, t)) => (n, convertType t)) slots)
  end
end
