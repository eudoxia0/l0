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
  open SymTab

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

  fun binopName Add = "+"
    | binopName Sub = "-"
    | binopName Mul = "*"
    | binopName Div = "/"
    | binopName Eq = "="
    | binopName NEq = "<>"
    | binopName LT = "<"
    | binopName LEq = "<="
    | binopName GT = ">"
    | binopName GEq = ">="

  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of string
               | Binop of binop * ast * ast
               | Cond of ast * ast * ast
               | Cast of Type.ty * ast
               | Progn of ast list
               | Let of string * ast * ast
               | Assign of string * ast
               | NullPtr of Parser.sexp
               | Load of ast
               | Store of ast * ast
               | Malloc of Parser.sexp * ast
               | Free of ast
               | AddressOf of string
               | Print of ast
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | While of ast * ast
               | Allocate of ast
               | MakeRecord of string * (string * ast) list
               | SlotAccess of ast * string
               | Funcall of string * ast list

  datatype top_ast = Defun of Function.func * ast
                   | Defrecord of string * (string * Type.ty) list

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
    and parseL "+" [a, b] e = Binop (Add, parse a e, parse b e)
      | parseL "-" [a, b] e = Binop (Sub, parse a e, parse b e)
      | parseL "*" [a, b] e = Binop (Mul, parse a e, parse b e)
      | parseL "/" [a, b] e = Binop (Div, parse a e, parse b e)
      | parseL "=" [a, b] e = Binop (Eq, parse a e, parse b e)
      | parseL "<>" [a, b] e = Binop (NEq, parse a e, parse b e)
      | parseL "<" [a, b] e = Binop (LT, parse a e, parse b e)
      | parseL "<=" [a, b] e = Binop (LEq, parse a e, parse b e)
      | parseL ">" [a, b] e = Binop (GT, parse a e, parse b e)
      | parseL ">=" [a, b] e = Binop (GEq, parse a e, parse b e)
      | parseL "if" [t, c, a] e = Cond (parse t e, parse c e, parse a e)
      | parseL "the" [t, a] e = Cast (Type.parseTypeSpecifier t e, parse a e)
      | parseL "progn" rest e = Progn (mparse rest e)
      | parseL "let" ((List [List [Symbol var, v]])::body) e =
        Let (var, parse v e, Progn (mparse body e))
      | parseL "let" ((List ((List [Symbol var, v])::rest))::body) e =
        let val exp = List [Symbol "let", List [List [Symbol var, v]],
                             List ((Symbol "let")::(List rest)::body)]
        in
            parse exp e
        end
      | parseL "let" ((List nil)::body) e =
        Progn (mparse body e)
      | parseL "<-" [Symbol var, v] e = Assign (var, parse v e)
      | parseL "c/nullptr" [t] _ = NullPtr t
      | parseL "load" [v] e = Load (parse v e)
      | parseL "store" [p, v] e = Store (parse p e, parse v e)
      | parseL "c/malloc" [t, c] e = Malloc (t, parse c e)
      | parseL "c/free" [p] e = Free (parse p e)
      | parseL "c/address-of" [Symbol v] _ = AddressOf v
      | parseL "print" [v] e = Print (parse v e)
      | parseL "c/embed" [t, String s] _ = CEmbed (t, s)
      | parseL "c/call" (String n :: t :: args) e = CCall (n, t, mparse args e)
      | parseL "while" (t :: body) e = While (parse t e, Progn (mparse body e))
      | parseL "allocate" [v] e = Allocate (parse v e)
      | parseL "not" [v] e = Funcall ("interim_not", [parse v e])
      | parseL "record" (Symbol name :: slots) e = MakeRecord (name, map (parseSlot e) slots)
      | parseL "slot" [r, Symbol slot] e = SlotAccess (parse r e, slot)
      | parseL f rest e = Funcall (f, mparse rest e)
    and mparse l e = map (fn elem => parse elem e) l
    and parseSlot e (List [Symbol name, exp]) = (name, parse exp e)
      | parseSlot e _ = raise Fail "Bad slot"

    fun parseParam (List [Symbol n, t]) e = Function.Param (n, Type.parseTypeSpecifier t e)
      | parseParam _ _ = raise Fail "Bad parameter"

    fun parseToplevel (List (Symbol f :: rest)) e = parseTopL f rest e
      | parseToplevel _ _ = raise Fail "Bad toplevel node"
    and parseTopL "defun" (Symbol name :: List params :: rt :: body) e =
        Defun (Function.Function (name,
                                  map (fn p => parseParam p e) params,
                                  Type.parseTypeSpecifier rt e),
               parse (List (Symbol "progn" :: body)) e)
      | parseTopL "defrecord" (Symbol name :: slots) e =
        Defrecord (name, (map (parseSlot e) slots))
      | parseTopL f _ _ = raise Fail ("Bad toplevel definition '" ^ f ^ "'")
    and parseSlot e (List [Symbol name, tys]) = (name, Type.parseTypeSpecifier tys e)
      | parseSlot e _ = raise Fail "Bad defrecord slot"
  end
end
