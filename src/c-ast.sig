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

signature C_AST = sig
  datatype ty = Bool
              | UInt8
              | Int8
              | UInt16
              | Int16
              | UInt32
              | Int32
              | UInt64
              | Int64
              | Pointer of ty
              | Struct of string

  datatype exp_ast = ConstBool of bool
                   | ConstInt of int
                   | ConstString of string
                   | ConstNull
                   | Var of string
                   | Binop of Binop.binop * exp_ast * exp_ast
                   | Cast of ty * exp_ast
                   | Deref of exp_ast
                   | AddressOf of exp_ast
                   | SizeOf of ty
                   | StructInitializer of string * (string * exp_ast) list
                   | StructAccess of exp_ast * string
                   | Adjacent of exp_ast list
                   | Raw of string

  datatype block_ast = Sequence of block_ast list
                     | Block of block_ast list
                     | Declare of ty * string
                     | Assign of exp_ast * exp_ast
                     | Cond of exp_ast * block_ast * block_ast
                     | While of exp_ast * block_ast
                     | Funcall of string option * string * exp_ast list

  datatype top_ast = FunctionDef of string * param list * ty * block_ast * exp_ast
                   | StructDef of string * slot list
       and param = Param of string * ty
       and slot = Slot of string * ty

  val renderTop : top_ast -> string
end
