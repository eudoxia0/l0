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
               | Var of Ident.ident
               | Cast of Type.ty * ast
               | Let of Ident.ident * ast * ast
               | Bind of Ident.ident list * ast * ast
               | NullPtr of Parser.sexp
               | Malloc of Parser.sexp * ast
               | AddressOf of Ident.ident
               | CEmbed of Parser.sexp * string
               | CCall of string * Parser.sexp * ast list
               | Operation of string * ast list

  (* Fresh identifiers *)

  val count = ref 0

  fun freshVar name =
    let
    in
        count := !count + 1;
        Ident.mkIdent name (!count)
    end

  fun resetCount () =
    count := 0

  (* Alpha renaming *)

  type stack = (string, Ident.ident) Map.map

  fun alphaRename (AST.Defun (name, ps, rt, body)) =
    let
    in
      resetCount ();
      let val (stack, params) = renameParams ps
      in
          let val (ast', _) = rename body stack
          in
              (Function.Function (name, params, rt), ast')
          end
      end
    end
  and renameParams ps =
    let val ps' = map (fn (AST.Param (n, t)) => Function.Param (freshVar n, t)) ps
    in
        (Map.iaddList Map.empty
                      (map (fn (Function.Param (i, _)) => (Ident.identName i, i))
                           ps'),
         ps')
    end
  and rename AST.ConstUnit s = (ConstUnit, s)
    | rename (AST.ConstBool b) s = (ConstBool b, s)
    | rename (AST.ConstInt i) s = (ConstInt i, s)
    | rename (AST.ConstString str) s = (ConstString str, s)
    | rename (AST.Var name) s =
      (case Map.get s name of
           SOME name' => (Var name', s)
         | NONE => raise Fail ("No variable with this name: " ^ name))
    | rename (AST.Cast (t, a)) s =
      let val (a', s') = rename a s
      in
          (Cast (t, a'), s')
      end
    | rename (AST.Let (name, exp, body)) s =
      let val (exp', s') = rename exp s
      in
          let val fresh = freshVar name
          in
              let val s'' = Map.iadd s' (name, fresh)
              in
                  let val (body', s''') = rename body s''
                  in
                      (Let (fresh, exp', body'), s''')
                  end
              end
          end
      end
    | rename (AST.Bind (binds, tup, body)) s =
      let val (tup', s') = rename tup s
      in
        let val freshBinds = map (fn b => (b, freshVar b)) binds
        in
          let val s'' = Map.iaddList s' freshBinds
          in
            let val (body', s''') = rename body s''
            in
                (Bind (map (fn (_, f) => f) freshBinds, tup', body'), s''')
            end
          end
        end
      end
    | rename (AST.NullPtr t) s = (NullPtr t, s)
    | rename (AST.Malloc (t, e)) s =
      let val (e', s') = rename e s
      in
          (Malloc (t, e'), s')
      end
    | rename (AST.AddressOf name) s =
      (case Map.get s name of
           SOME name' => (AddressOf name', s)
         | NONE => raise Fail ("No variable with this name: " ^ name))
    | rename (AST.CEmbed (t, e)) s = (CEmbed (t, e), s)
    | rename (AST.CCall (name, ty, l)) s =
      let val (args', s') = renameList l s
      in
          (CCall (name, ty, args'), s')
      end
    | rename (AST.Operation (oper, l)) s =
      let val (args', s') = renameList l s
      in
          (Operation (oper, args'), s')
      end
  and renameList (head::tail) s =
      let val (head', s') = rename head s
      in
          let val (list, s'') = (renameList tail s')
          in
              (head' :: list, s'')
          end
      end
    | renameList nil s = (nil, s)

  fun toString ConstUnit = "nil"
    | toString (ConstBool true) = "true"
    | toString (ConstBool false) = "false"
    | toString (ConstInt i) = (Int.toString i)
    | toString (ConstString s) = "\"" ^ s ^ "\""
    | toString (Var i) = Ident.toString i
    | toString (Cast (t, e)) =
      "(cast " ^ (Type.toString t) ^ " " ^ (toString e) ^ ")"
    | toString (Let (i, v, b)) =
      "(let ((" ^ (Ident.toString i) ^ " " ^ (toString v) ^ ")) " ^ (toString b) ^ ")"
    | toString (Bind (ids, t, b)) =
      "(bind (" ^ (String.concatWith " " (map Ident.toString ids)) ^ ") "
      ^ (toString t) ^ " " ^ (toString b) ^ ")"
    | toString (NullPtr t) =
      "c/nullptr"
    | toString (Malloc _) =
      "c/malloc"
    | toString (AddressOf _) =
      "c/address-of"
    | toString (CEmbed _) =
      "c/embed"
    | toString (CCall _) =
      "c/call"
    | toString (Operation (name, args)) =
      "(fn:" ^ name ^ " " ^ (String.concatWith " " (map toString args)) ^ ")"
end
