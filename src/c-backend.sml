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

structure CBackend :> C_BACKEND = struct
  type tuple_types = CAst.ty OrderedSet.set

  datatype context = Context of CAst.top_ast list * tuple_types

  val emptyContext = Context ([], OrderedSet.empty)

  fun renderContext (Context (ts, _)) =
      String.concatWith "\n" (map CAst.renderTop ts)


  (* Extract tuple types from TAST expressions *)

  local
    open TAST
    open Set
  in
    fun allTypes (TBinop (_, lhs, rhs, ty)) = unionList [allTypes lhs,
                                                         allTypes rhs,
                                                         singleton ty]
      | allTypes (TCond (t, c, a, ty)) = unionList [allTypes t,
                                                    allTypes c,
                                                    allTypes a,
                                                    singleton ty]
      | allTypes (TCast (ty, exp)) = add (allTypes exp) ty
      | allTypes (TProgn l) = unionList (map allTypes l)
      | allTypes (TLet (_, v, exp)) = union (allTypes v) (allTypes exp)
      | allTypes (TAssign (_, exp)) = allTypes exp
      | allTypes (TNullPtr ty) = singleton ty
      | allTypes (TLoad (exp, ty)) = add (allTypes exp) ty
      | allTypes (TStore (ptr, exp)) = union (allTypes ptr) (allTypes exp)
      | allTypes (TMalloc (ty, exp)) = add (allTypes exp) ty
      | allTypes (TFree exp) = allTypes exp
      | allTypes (TPrint exp) = allTypes exp
      | allTypes (TCEmbed (ty, _)) = singleton ty
      | allTypes (TCCall (_, ty, args)) = add (unionList (map allTypes args)) ty
      | allTypes (TWhile (test, body)) = union (allTypes test) (allTypes body)
      | allTypes (TFuncall (_, args, ty)) = add (unionList (map allTypes args)) ty
      | allTypes exp = add empty (typeOf exp)
  end

  local
    open Type
  in
    fun filterTuples l =
        List.filter (fn ty => case ty of
                                  (Tuple _) => true
                                | _ => false)
                    l
  end

  fun collectTupleTypes tuple_types tast =
    Set.addList tuple_types (filterTuples (allTypes tast))
end
