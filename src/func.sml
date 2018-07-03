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

  datatype binding = Binding of string * ty * mutability
  type stack = binding SymTab.symtab

  fun bindType (Binding (s, t, _)) = t

  fun funcName (Function (n, _, _)) = n

  fun funcRT (Function (_, _, r)) = r



  fun toStack (Function (_, params, _)) =
    let fun toStack' (Param (n,t)::rest) acc = bind (n, Binding (n, forciblyConcretizeType t, Immutable))
                                                        (toStack' rest acc)
          | toStack' nil acc = acc

    in
        toStack' params empty
    end
end
