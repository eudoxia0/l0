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
  datatype binop = Add
                 | Sub
                 | Mul
                 | Div
                 | Eq
                 | NEq
                 | LT
                 | LEq
                 | GT
                 | GEq

  val binopName : binop -> string

  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Cast of Type.ty * ast
               | Let of string * ast * ast
               | NullPtr of Parser.sexp
               | Malloc of Parser.sexp * ast
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list

  datatype top_ast = Defun of Function.func * ast
                   | Defrecord of string * (string * Type.ty) list

  val parse : Parser.sexp -> Type.tenv -> ast
  val parseToplevel : Parser.sexp -> Type.tenv -> top_ast
end
