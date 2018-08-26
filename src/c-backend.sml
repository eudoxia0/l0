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
  datatype context = Context of CAst.top_ast list * CAst.ty OrderedSet.set

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
      | allTypes exp = add empty (typeOf exp)
  end
end
