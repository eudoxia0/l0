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

signature AST = sig
  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Cast of Type.ty * ast
               | Let of string * ast * ast
               | NullPtr of Parser.sexp
               | Malloc of Parser.sexp * ast
               | AddressOf of string
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | Operation of string * ast list

  datatype top_ast = Defun of string * param list * Type.ty * ast
       and param = Param of string * Type.ty

  val parse : Parser.sexp -> Type.tenv -> ast
  val parseToplevel : Parser.sexp -> Type.tenv -> top_ast
end
