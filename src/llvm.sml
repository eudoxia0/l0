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
    | renderType (Struct ts) = "{ " ^ (String.concatWith ", " (map renderType ts)) ^ " }"

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
                   | IntConstant of string

  datatype operation = Copy of ty * operand
                     | Add of ty * operand * operand
                     | Sub of ty * operand * operand
                     | Mul of ty * operand * operand
                     | UDiv of ty * operand * operand
                     | SDiv of ty * operand * operand
                     | ExtractValue of ty * operand * int
                     | InsertValue of ty * operand * ty * operand * int

  datatype instruction = UnconditionalBranch of label
                       | ConditionalBranch of operand * label * label
                       | Assignment of register * operation
                       | Return of ty * operand

  fun renderOperand (RegisterOp r) = renderRegister r
    | renderOperand (IntConstant s) = s

  fun renderOperation (Copy (t, v)) = (renderType t) ^ " " ^ (renderOperand v)
    | renderOperation (Add (t, l, r)) = renderArithOp "add" t l r
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
  and renderArithOp oper t l r = oper ^ " " ^ (renderType t) ^ " " ^ (renderOperand l) ^ ", " ^ (renderOperand r)

  fun renderInstruction (UnconditionalBranch l) =
      "br label " ^ (renderLabel l)
    | renderInstruction (ConditionalBranch (c, t, f)) =
      "br i1 " ^ (renderOperand c) ^ ", label " ^ (renderLabel t) ^ ", label " ^ (renderLabel f)
    | renderInstruction (Assignment (r, oper)) =
      (renderRegister r) ^ " = " ^ (renderOperation oper)
    | renderInstruction (Return (t, v)) =
      "ret " ^ (renderType t) ^ " " ^ (renderOperand v)

  (* Toplevel *)

  type global_var = string

  datatype toplevel = StringConstant of global_var * string
                    | FunctionDeclaration of string * ty * param_decl list
                    | FunctionDefinition of string * ty * param list * instruction list
       and param_decl = ParamDecl of ty
       and param = Param of string * ty

  fun renderToplevel (StringConstant (name, str)) =
      "@" ^ name ^ " = private unnamed_addr constant [" ^ (Int.toString (String.size str + 1)) ^ " x i8] c\"" ^ str ^ "\\00\""
    | renderToplevel (FunctionDeclaration (name, rt, params)) =
      "declare " ^ (renderType rt) ^ " @" ^ name ^ "(" ^ (String.concatWith ", " (map renderParamDecl params)) ^ ")"
    | renderToplevel (FunctionDefinition (name, rt, params, insts)) =
      "define " ^ (renderType rt) ^ " @" ^ name ^ "(" ^ (String.concatWith ", " (map renderParam params)) ^ ") {\n  " ^ (String.concatWith "\n  " (map renderInstruction insts)) ^ "\n}"
  and renderParamDecl (ParamDecl t) = renderType t
  and renderParam (Param (name, t)) = name ^ (renderType t)
end
