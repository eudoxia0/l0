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

structure OAST :> OAST = struct
  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of NameGen.name
               | Binop of Binop.binop * ast * ast
               | Cond of ast * ast * ast
               | Cast of Type.ty * ast
               | Progn of ast list
               | Let of NameGen.name * ast * ast
               | Assign of NameGen.name * ast
               | NullPtr of Parser.sexp
               | Load of ast
               | Store of ast * ast
               | Malloc of Parser.sexp * ast
               | Free of ast
               | AddressOf of NameGen.name
               | Print of ast
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | While of ast * ast
               | Allocate of ast
               | MakeRecord of string * (string * ast) list
               | SlotAccess of ast * string
               | Funcall of string * ast list

  fun augment ARAST.ConstUnit = ConstUnit
    | augment (ARAST.ConstBool b) = ConstBool b
    | augment (ARAST.ConstInt i) = ConstInt i
    | augment (ARAST.ConstString s) = ConstString s
    | augment (ARAST.Var n) = Var n
    | augment (ARAST.Cast (t, e)) = Cast (t, augment e)
    | augment (ARAST.Let (n, e, b)) = Let (n, augment e, augment b)
    | augment (ARAST.NullPtr t) = NullPtr t
    | augment (ARAST.Malloc (t, e)) = Malloc (t, augment e)
    | augment (ARAST.AddressOf n) = AddressOf n
    | augment (ARAST.CEmbed (t, s)) = CEmbed (t, s)
    | augment (ARAST.CCall (n, t, args)) = CCall (n, t, map augment args)
    | augment (ARAST.Operation (name, args)) = augmentOp name (map augment args)
  and augmentOp "if" [t, c, e] = Cond (t, c, e)
    | augmentOp "if" _ = raise Fail "Bad if form"
    | augmentOp "+" [l, r] = Binop (Binop.Add, l, r)
    | augmentOp "-" [l, r] = Binop (Binop.Sub, l, r)
    | augmentOp "*" [l, r] = Binop (Binop.Mul, l, r)
    | augmentOp "/" [l, r] = Binop (Binop.Div, l, r)
    | augmentOp "progn" args = Progn args
    | augmentOp "load" [e] = Load e
    | augmentOp "load" _ = raise Fail "Bad load form"
    | augmentOp "store" [p, v] = Store (p, v)
    | augmentOp "store" _ = raise Fail "Bad store form"
    | augmentOp "free" [p] = Free p
    | augmentOp "free" _ = raise Fail "Bad free form"
    | augmentOp "print" [v] = Print v
    | augmentOp "print" _ = raise Fail "Bad print form"
    | augmentOp "while" (t::body) = While (t, Progn body)
    | augmentOp "while" _ = raise Fail "Bad while form"
    | augmentOp "allocate" [v] = Allocate v
    | augmentOp "allocate" _ = raise Fail "Bad allocate form"
    | augmentOp name args = Funcall (name, args)
end
