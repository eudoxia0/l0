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

structure ARAST :> ARAST = struct
  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of int
               | Binop of AST.binop * ast * ast
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

  datatype namegen = NameGen of int

  fun freshName (NameGen i) =
    (i + 1, NameGen (i + 1))

  datatype bind = Binding of string * int

  type stack = bind SymTab.symtab

  fun alphaRename ast =
    let val (ast', _, _) = rename ast SymTab.empty (NameGen 1)
    in
        ast'
    end
  and rename AST.ConstUnit s n = (ConstUnit, s, n)
    | rename (AST.ConstBool b) s n = (ConstBool b, s, n)
    | rename (AST.ConstInt i) s n = (ConstInt i, s, n)
    | rename (AST.ConstString str) s n = (ConstString str, s, n)
    | rename (AST.Var name) s n = (Var (SymTab.lookup name s), s, n)
    | rename (AST.Binop (b, l, r)) s n =
      let val (l', s', n') = rename l s n
      in
          let val (r', s'', n'') = rename r s' n'
          in
              (Binop (b, l', r'), s'', n'')
          end
      end
  and renameList (head::tail) s n =
      let val (head', s', n') = rename head s n
      in
          let val (list, s'', n'') = (renameList tail s' n')
          in
              (head' :: list, s'', n'')
          end
      end
    | renameList nil s n = (nil, s, n)
end
