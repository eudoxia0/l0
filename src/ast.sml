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

structure AST :> AST = struct
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

  local
    open Parser
  in
    fun parse (Integer i) _ = ConstInt i
      | parse (String s) _ = ConstString s
      | parse (Symbol "nil") _ = ConstUnit
      | parse (Symbol "true") _ = ConstBool true
      | parse (Symbol "false") _ = ConstBool false
      | parse (Symbol s) _ = Var s
      | parse (List (Symbol f::rest)) e = parseL f rest e
      | parse _ _ = raise Fail "Bad expression"
    and parseL "the" [t, a] e = Cast (Type.parseTypeSpecifier t e, parse a e)
      | parseL "progn" rest e = Operation ("progn", mparse rest e)
      | parseL "let" ((List [List [Symbol var, v]])::body) e =
        Let (var, parse v e, Operation ("progn", mparse body e))
      | parseL "let" ((List ((List [Symbol var, v])::rest))::body) e =
        let val exp = List [Symbol "let", List [List [Symbol var, v]],
                             List ((Symbol "let")::(List rest)::body)]
        in
            parse exp e
        end
      | parseL "let" ((List nil)::body) e =
        Operation ("progn", mparse body e)
      | parseL "c/nullptr" [t] _ = NullPtr t
      | parseL "c/malloc" [t, c] e = Malloc (t, parse c e)
      | parseL "c/address-of" [Symbol s] _ = AddressOf s
      | parseL "c/embed" [t, String s] _ = CEmbed (t, s)
      | parseL "c/call" (String n :: t :: args) e = CCall (n, t, mparse args e)
      | parseL f rest e = Operation (f, mparse rest e)
    and mparse l e = map (fn elem => parse elem e) l

    fun parseToplevel (List (Symbol f :: rest)) e = parseTopL f rest e
      | parseToplevel _ _ = raise Fail "Bad toplevel node"
    and parseTopL "defun" (Symbol name :: List params :: rt :: body) e =
      Defun (name,
             map (fn p => parseParam p e) params,
             Type.parseTypeSpecifier rt e,
             parse (List (Symbol "progn" :: body)) e)
      | parseTopL f _ _ = raise Fail ("Bad toplevel definition '" ^ f ^ "'")
    and parseParam (List [Symbol n, t]) e = Param (n, Type.parseTypeSpecifier t e)
      | parseParam _ _ = raise Fail "Bad parameter"
  end
end
