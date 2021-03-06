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

structure LLVM :> LLVM = struct
  (* Rendering utils *)

  fun commaSep f l =
    String.concatWith ", " (map f l)

  (* LLVM Types *)

  datatype ty = Bool
              | Int8
              | Int16
              | Int32
              | Int64
              | SingleFloat
              | DoubleFloat
              | Pointer of ty
              | FuncPointer of ty * ty list
              | Struct of ty list
       and bit_width = Word1 | Word8 | Word16 | Word32 | Word64
       and float_width = Single | Double

  fun renderType Bool = "i1"
    | renderType Int8 = "i8"
    | renderType Int16 = "i16"
    | renderType Int32 = "i32"
    | renderType Int64 = "i64"
    | renderType SingleFloat = "float"
    | renderType DoubleFloat = "double"
    | renderType (Pointer t) = (renderType t) ^ "*"
    | renderType (FuncPointer (rt, ts)) =
      let val rt' = renderType rt
          and ts' = String.concatWith " " (map renderType ts)
      in
          rt' ^ " (" ^ ts' ^ ")*"
      end
    | renderType (Struct ts) = "{ " ^ (commaSep renderType ts) ^ " }"

  (* Registers *)

  datatype register = Register of int

  datatype register_names = RegisterNames of int

  fun freshRegister (RegisterNames i) =
    let val i' = i + 1
    in
        (Register i', RegisterNames i')
    end

  fun renderRegister (Register i) = "%" ^ (Int.toString i)

  (* Labels *)

  datatype label = Label of int

  datatype label_names = LabelNames of int

  fun freshLabel (LabelNames i) =
    let val i' = i + 1
    in
        (Label i', LabelNames i')
    end

  fun renderLabel (Label i) = "L" ^ (Int.toString i) ^ ":"

  (* Instructions *)

  datatype operand = RegisterOp of register
                   | NamedRegister of string
                   | IntConstant of string

  datatype comp_op = Eq
                   | NotEq
                   | UnsignedGT
                   | UnsignedGEq
                   | UnsignedLT
                   | UnsignedLEq
                   | SignedGT
                   | SignedGEq
                   | SignedLT
                   | SignedLEq

  datatype phi_pred = PhiPred of operand * label

  datatype arg = Argument of ty * operand

  datatype operation = Add of ty * operand * operand
                     | Sub of ty * operand * operand
                     | Mul of ty * operand * operand
                     | UDiv of ty * operand * operand
                     | SDiv of ty * operand * operand
                     | ExtractValue of ty * operand * int
                     | InsertValue of ty * operand * ty * operand * int
                     | Load of ty * operand (* ty is the type of the result, not the pointer *)
                     | IntegerCompare of comp_op * ty * operand * operand
                     | Phi of ty * phi_pred list
                     | DirectCall of string * ty * arg list
                     | Alloca of ty

  datatype instruction = UnconditionalBranch of label
                       | ConditionalBranch of operand * label * label
                       | Assignment of register * operation
                       | Return of ty * operand
                       (* below: ty is the type of the result, not the
                          pointer. the first operand is the value, the second is
                          the location. *)
                       | Store of ty * operand * operand

  fun renderOperand (RegisterOp r) = renderRegister r
    | renderOperand (NamedRegister n) = "%" ^ n
    | renderOperand (IntConstant s) = s

  fun renderPhi (PhiPred (oper, l)) =
    (renderOperand oper) ^ ", " ^ (renderLabel l)

  fun renderArg (Argument (t, oper)) =
    (renderType t) ^ ", " ^ (renderOperand oper)

  fun renderCompOp Eq = "eq"
    | renderCompOp NotEq = "ne"
    | renderCompOp UnsignedGT = "ugt"
    | renderCompOp UnsignedGEq = "uge"
    | renderCompOp UnsignedLT = "ult"
    | renderCompOp UnsignedLEq = "ule"
    | renderCompOp SignedGT = "sgt"
    | renderCompOp SignedGEq = "sge"
    | renderCompOp SignedLT = "slt"
    | renderCompOp SignedLEq = "sle"

  fun renderOperation (Add (t, l, r)) = renderArithOp "add" t l r
    | renderOperation (Sub (t, l, r)) = renderArithOp "sub" t l r
    | renderOperation (Mul (t, l, r)) = renderArithOp "mul" t l r
    | renderOperation (UDiv (t, l, r)) = renderArithOp "udiv" t l r
    | renderOperation (SDiv (t, l, r)) = renderArithOp "sdiv" t l r
    | renderOperation (ExtractValue (t, v, idx)) =
      "extractvalue " ^ (renderType t) ^ " " ^ (renderOperand v) ^ ", " ^ (Int.toString idx)
    | renderOperation (InsertValue (t, v, elemt, elemv, idx)) =
      "insertvalue " ^ (renderType t) ^ " " ^ (renderOperand v)
      ^ ", " ^ (renderType elemt) ^ " " ^ (renderOperand v)
      ^ ", " ^ (Int.toString idx)
    | renderOperation (Load (t, ptr)) =
      "load " ^ (renderType t) ^ ", " ^ (renderType (Pointer t)) ^ " " ^ (renderOperand ptr)
    | renderOperation (IntegerCompare (oper, t, l, r)) =
      "icmp " ^ (renderCompOp oper) ^ " " ^ (renderType t) ^ " " ^ (renderOperand l)
      ^ ", " ^ (renderOperand r)
    | renderOperation (Phi (t, preds)) =
      "phi " ^ (renderType t) ^ " " ^ (commaSep renderPhi preds)
    | renderOperation (DirectCall (name, rt, args)) =
      "call " ^ (renderType rt) ^ " @" ^ name ^ "(" ^ (commaSep renderArg args) ^ ")"
    | renderOperation (Alloca t) =
      "alloca " ^ (renderType t)
  and renderArithOp oper t l r =
      oper ^ " " ^ (renderType t) ^ " " ^ (renderOperand l) ^ ", " ^ (renderOperand r)

  fun renderInstruction (UnconditionalBranch l) =
      "br label " ^ (renderLabel l)
    | renderInstruction (ConditionalBranch (c, t, f)) =
      "br i1 " ^ (renderOperand c) ^ ", label " ^ (renderLabel t) ^ ", label " ^ (renderLabel f)
    | renderInstruction (Assignment (r, oper)) =
      (renderRegister r) ^ " = " ^ (renderOperation oper)
    | renderInstruction (Return (t, v)) =
      "ret " ^ (renderType t) ^ " " ^ (renderOperand v)
    | renderInstruction (Store (t, v, ptr)) =
      "store " ^ (renderType t) ^ " " ^ (renderOperand v)
      ^ ", " ^ (renderType (Pointer t)) ^ " " ^ (renderOperand ptr)

  (* Toplevel *)

  type global_var = string

  datatype toplevel = StringConstant of global_var * string
                    | FunctionDeclaration of string * ty * param_decl list
                    | FunctionDefinition of string * ty * param list * instruction list
       and param_decl = ParamDecl of ty
       and param = Param of string * ty

  fun renderToplevel (StringConstant (name, str)) =
      let val size = Int.toString (String.size str + 1)
          and str' = "\"" ^ str ^ "\\00\""
      in
          "@" ^ name ^ " = private unnamed_addr constant [" ^ size ^ " x i8] c" ^ str'
      end
    | renderToplevel (FunctionDeclaration (name, rt, params)) =
      let val params' = commaSep renderParamDecl params
      in
          "declare " ^ (renderType rt) ^ " @" ^ name ^ "(" ^ params' ^ ")"
      end
    | renderToplevel (FunctionDefinition (name, rt, params, insts)) =
      let val params' = commaSep renderParam params
          and body = String.concatWith "\n  " (map renderInstruction insts)
      in
          "define " ^ (renderType rt) ^ " @" ^ name ^ "(" ^ params' ^ ") {\n  " ^ body ^ "\n}"
      end
  and renderParamDecl (ParamDecl t) = renderType t
  and renderParam (Param (name, t)) = name ^ (renderType t)
end
