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
  datatype func = Function of string * param list * Type.ty
       and param = Param of Ident.ident * Type.ty

  type fenv = (string, func) Map.map

  val funcName : func -> string
  val funcRT : func -> Type.ty

  val matchParams : param list -> Type.ty list -> bool
end
