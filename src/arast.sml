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
  open NameGen

  datatype ast = ConstUnit
               | ConstBool of bool
               | ConstInt of int
               | ConstString of string
               | Var of NameGen.name
               | Cast of Type.ty * ast
               | Let of NameGen.name * ast * ast
               | NullPtr of Parser.sexp
               | Malloc of Parser.sexp * ast
               | AddressOf of NameGen.name
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | Operation of string * ast list

  type renamed = (string, NameGen.name) Map.map

  fun alphaRename ast ng =
    let val (ast', stack, _) = rename ast Map.empty ng
    in
        (ast', stack)
    end
  and rename AST.ConstUnit s n = (ConstUnit, s, n)
    | rename (AST.ConstBool b) s n = (ConstBool b, s, n)
    | rename (AST.ConstInt i) s n = (ConstInt i, s, n)
    | rename (AST.ConstString str) s n = (ConstString str, s, n)
    | rename (AST.Var name) s n =
      (case Map.get s name of
           SOME name' => (Var name', s, n)
         | NONE => raise Fail ("No variable with this name: " ^ name))
    | rename (AST.Cast (t, a)) s n =
      let val (a', s', n') = rename a s n
      in
          (Cast (t, a'), s', n')
      end
    | rename (AST.Let (name, exp, body)) s n =
      let val (exp', s', n') = rename exp s n
      in
          let val (fresh, n'') = freshName n'
          in
              let val s'' = Map.iadd s (name, fresh)
              in
                  let val (body', s''', n''') = rename body s' n'
                  in
                      (Let (fresh, exp', body'), s''', n''')
                  end
              end
          end
      end
    | rename (AST.NullPtr t) s n = (NullPtr t, s, n)
    | rename (AST.Malloc (t, e)) s n =
      let val (e', s', n') = rename e s n
      in
          (Malloc (t, e'), s', n')
      end
    | rename (AST.AddressOf name) s n =
      (case Map.get s name of
           SOME name' => (AddressOf name', s, n)
         | NONE => raise Fail ("No variable with this name: " ^ name))
    | rename (AST.CEmbed (t, e)) s n = (CEmbed (t, e), s, n)
    | rename (AST.CCall (name, ty, l)) s n =
      let val (args', s', n') = renameList l s n
      in
          (CCall (name, ty, args'), s', n')
      end
    | rename (AST.Operation (oper, l)) s n =
      let val (args', s', n') = renameList l s n
      in
          (Operation (oper, args'), s', n')
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

  fun alphaRenameParams params ng =
      let val stack = Map.empty
      in
          let val (_, stack', ng') = renameParamList params stack ng
          in
              (stack', ng')
          end
      end
  and renameParam (Function.Param (name, ty)) stack ng =
      let val (fresh, ng') = NameGen.freshName ng
      in
          ((), Map.iadd stack (name, fresh), ng')
      end
  and renameParamList (head::tail) s n =
      let val (head', s', n') = renameParam head s n
      in
          let val (list, s'', n'') = (renameParamList tail s' n')
          in
              (head' :: list, s'', n'')
          end
      end
    | renameParamList nil s n = (nil, s, n)
end
