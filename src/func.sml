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

structure Function :> FUNCTION = struct
  open SymTab
  open Type

  datatype param = Param of string * ty
  datatype conc_param = ConcParam of string * Type.ty
  datatype func = Function of string * param list * ty

  type fenv = func SymTab.symtab

  datatype mutability = Mutable
                      | Immutable

  datatype binding = Binding of Type.ty * mutability
  type stack = (int, binding) Map.map

  fun bindType (Binding (t, _)) = t

  fun funcName (Function (n, _, _)) = n

  fun funcRT (Function (_, _, r)) = r

  fun matchParams params argtypes =
    if (length params <> length argtypes) then
        raise Fail "Wrong parameter count"
    else
        ListPair.all (fn (pt, at) => pt = at)
                     (map (fn (Param (n,t)) => t) params, argtypes)

  local
    open NameGen
  in
    datatype param_int = ParamInt of int * ty

    fun alphaRenameParams (head::tail) ng =
      let val (head', ng') = alphaRenameParam head ng
      in
          let val (list, ng'') = (alphaRenameParams tail ng')
          in
              (head' :: list, ng'')
          end
      end
    and alphaRenameParam (Param (_, ty)) ng =
        let val (i, ng') = freshName ng
        in
            (ParamInt (i, ty), ng')
        end

    fun toStack (Function (_, params, _)) =
      let val ng = NameGen 1
      in
          let val (params', ng') = alphaRenameParams params ng
          in
              let fun inner ((ParamInt (_, t))::tail) acc = Map.iadd acc (i, Binding (t, Immutable))
                    | inner nil acc = acc
              in
                  (inner params' Map.empty, ng')
              end
          end
      end
  end
end
