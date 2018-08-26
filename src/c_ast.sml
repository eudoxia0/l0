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

structure CAst :> C_AST = struct
  datatype ty = Bool
              | UInt8
              | Int8
              | UInt16
              | Int16
              | UInt32
              | Int32
              | UInt64
              | Int64
              | Pointer of ctype
              | Struct of string
              | RegionType

  datatype param = Param of string * ctype

  datatype exp_cast = CConstBool of bool
                    | CConstInt of int
                    | CConstString of string
                    | CConstNull
                    | CVar of string
                    | CBinop of AST.binop * exp_cast * exp_cast
                    | CCast of ctype * exp_cast
                    | CDeref of exp_cast
                    | CAddressOf of exp_cast
                    | CSizeOf of ctype
                    | CStructInitializer of string * (string * exp_cast) list
                    | CStructAccess of exp_cast * string
                    | CAdjacent of exp_cast list
                    | CRaw of string

  datatype block_cast = CSeq of block_cast list
                      | CBlock of block_cast list
                      | CDeclare of ctype * string
                      | CAssign of exp_cast * exp_cast
                      | CCond of exp_cast * block_cast * block_cast
                      | CWhile of exp_cast * block_cast
                      | CFuncall of string option * string * exp_cast list

  datatype top_cast = CFunction of string * cparam list * ctype * block_cast * exp_cast
                    | CStructDef of string * (string * ctype) list
end
