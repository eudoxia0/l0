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

structure Backend :> BACKEND = struct
  open LLVM

  (* Context *)

  datatype context = Context of toplevel list

  val emptyContext = Context []

  fun renderContext (Context ts) =
      String.concatWith "\n\n" (map LLVM.renderToplevel ts)

  (* Backend *)

  type compiled_exp = LLVM.instruction list * LLVM.operand * LLVM.register_names * LLVM.label_names

  fun variableRegister name =
    "_var_" ^ (Int.toString (NameGen.nameId name))

  fun mapTy Type.Unit = Bool
    | mapTy Type.Bool = Bool
    | mapTy (Type.Int (Type.Unsigned, Type.Word8)) = Int8
    | mapTy (Type.Int (Type.Signed, Type.Word8)) = Int8
    | mapTy (Type.Int (Type.Unsigned, Type.Word16)) = Int16
    | mapTy (Type.Int (Type.Signed, Type.Word16)) = Int16
    | mapTy (Type.Int (Type.Unsigned, Type.Word32)) = Int32
    | mapTy (Type.Int (Type.Signed, Type.Word32)) = Int32
    | mapTy (Type.Int (Type.Unsigned, Type.Word64)) = Int64
    | mapTy (Type.Int (Type.Signed, Type.Word64)) = Int64
    | mapTy Type.Str = Pointer Int8
    | mapTy (Type.RawPointer t) = Pointer (mapTy t)
    | mapTy (Type.Tuple ts) = Struct (map mapTy ts)

  fun unitConstant rn ln = ([], IntConstant "0", rn, ln)

  local
    open TAST
  in
    fun compileExp TConstUnit rn ln = unitConstant rn ln
      | compileExp (TConstBool false) rn ln = ([], IntConstant "false", rn, ln)
      | compileExp (TConstBool true) rn ln = ([], IntConstant "true", rn, ln)
      | compileExp (TConstInt (i, _)) rn ln = ([], IntConstant (Int.toString i), rn, ln)
      | compileExp (TConstString _) _ _ = raise Fail "String support not implemented yet"
      | compileExp (TVar (name, ty)) rn ln = compileLoadVar name ty rn ln
      | compileExp (TBinop (Binop.Add, lhs, rhs, ty)) rn ln = compileArithOp Add lhs rhs ty rn ln
      | compileExp (TBinop (Binop.Sub, lhs, rhs, ty)) rn ln = compileArithOp Sub lhs rhs ty rn ln
      | compileExp (TBinop (Binop.Mul, lhs, rhs, ty)) rn ln = compileArithOp Mul lhs rhs ty rn ln
      | compileExp (TBinop (Binop.Div, lhs, rhs, ty)) rn ln = compileArithOp SDiv lhs rhs ty rn ln
      | compileExp (TBinop (Binop.Eq, lhs, rhs, ty)) rn ln = compileComparisonOp Eq lhs rhs ty rn ln
      | compileExp (TBinop (Binop.NEq, lhs, rhs, ty)) rn ln = compileComparisonOp NotEq lhs rhs ty rn ln
      | compileExp (TBinop (Binop.LT, lhs, rhs, ty)) rn ln = compileComparisonOp UnsignedLT lhs rhs ty rn ln
      | compileExp (TBinop (Binop.LEq, lhs, rhs, ty)) rn ln = compileComparisonOp UnsignedLEq lhs rhs ty rn ln
      | compileExp (TBinop (Binop.GT, lhs, rhs, ty)) rn ln = compileComparisonOp UnsignedGT lhs rhs ty rn ln
      | compileExp (TBinop (Binop.GEq, lhs, rhs, ty)) rn ln = compileComparisonOp UnsignedGEq lhs rhs ty rn ln
      | compileExp (TCond (t, c, a, ty)) rn ln = raise Fail "cond not implemented"
      | compileExp (TCast (ty, exp)) rn ln = raise Fail "cast not implemented"
      | compileExp (TLet _) rn ln = raise Fail "let not implemented"
      | compileExp (TAssign _) rn ln = raise Fail "assign not implemented"
      | compileExp (TProgn body) rn ln = compileProgn body rn ln
      | compileExp _ _ _ = raise Fail "Not implemented"
    and compileLoadVar name ty rn ln =
        let val (result, rn') = freshRegister rn
        in
            ([Assignment (result, Load (mapTy ty, NamedRegister (variableRegister name)))],
             RegisterOp result,
             rn',
             ln)
        end
    and compileArithOp oper lhs rhs ty rn ln =
        let val (insts, lhs', rn', ln') = compileExp lhs rn ln
        in
            let val (insts', rhs', rn'', ln'') = compileExp rhs rn' ln'
            in
                let val (result, rn''') = freshRegister rn
                in
                    let val insts'' = [Assignment (result, oper (mapTy ty, lhs', rhs'))]
                    in
                        (insts @ insts' @ insts'', RegisterOp result, rn''', ln'')
                    end
                end
            end
        end
    and compileComparisonOp oper lhs rhs ty rn ln =
        let val (insts, lhs', rn', ln') = compileExp lhs rn ln
        in
            let val (insts', rhs', rn'', ln'') = compileExp rhs rn' ln'
            in
                let val (result, rn''') = freshRegister rn
                in
                    let val insts'' = [Assignment (result, IntegerCompare (oper, mapTy ty, lhs', rhs'))]
                    in
                        (insts @ insts' @ insts'', RegisterOp result, rn''', ln'')
                    end
                end
            end
        end
    and compileProgn body rn ln =
        if body = nil then
            unitConstant rn ln
        else
            compileExps body rn ln
    and compileExpList (head::tail) rn ln =
        let val (insts, head', rn', ln') = compileExp head rn ln
        in
            (insts, head', rn', ln') :: (compileExpList tail rn' ln')
        end
      | compileExpList nil _ _ = []
    and compileExps ls rn ln =
        if (List.length ls) > 0 then
            let val elems = compileExpList ls rn ln
            in
                let val (_, operand, rn', ln') = List.last elems
                    and getInsts = fn (i, _, _, _) => i
                in
                    (List.concat (map getInsts elems), operand, rn', ln')
                end
            end
        else
            raise Fail "compileExps called with empty list"
  end

  fun compileFunc (Context ts) (Function.Function (name, params, rt)) tast =
      let val (insts, oper, _, _) = compileExp tast
                                               (RegisterNames 1)
                                               (LabelNames 1)
          and rtype = mapTy rt
      in
          let val fundef = FunctionDefinition (name,
                                               rtype,
                                               map mapParam params,
                                               insts @ [Return (rtype, oper)])
          in
              Context (ts @ [fundef])
          end
      end
  and mapParam (Function.Param (name, ty)) = Param (name, mapTy ty)
end
