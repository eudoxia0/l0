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

signature LLVM = sig
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

  datatype register = Register of int
  datatype register_names = RegisterNames of int

  datatype label = Label of int
  datatype label_names = LabelNames of int

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

  type global_var = string

  datatype toplevel = StringConstant of global_var * string
                    | FunctionDeclaration of string * ty * param_decl list
                    | FunctionDefinition of string * ty * param list * instruction list
       and param_decl = ParamDecl of ty
       and param = Param of string * ty

  type context

  val emptyContext : context
  val renderContext : context -> string
  val mapTy : Type.ty -> ty
  val compileExp : TAST.tast -> register_names -> label_names -> (instruction list * operand * register_names * label_names)
  val compileFunc : context -> Function.func -> TAST.tast -> context
end
