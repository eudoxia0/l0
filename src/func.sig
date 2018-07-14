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

signature FUNCTION = sig
  datatype param = Param of string * Type.ty
  datatype conc_param = ConcParam of string * Type.ty
  datatype func = Function of string * param list * Type.ty

  type fenv = func SymTab.symtab

  datatype mutability = Mutable
                      | Immutable

  datatype binding = Binding of Type.ty * mutability
  type stack = (int, binding) Map.map

  val bindType : binding -> Type.ty

  val funcName : func -> string
  val funcRT : func -> Type.ty

  val matchParams : param list -> Type.ty list -> bool

  val toStack : func -> (stack * NameGen.namegen)
end
