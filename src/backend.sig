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

signature BACKEND = sig
  type context

  val emptyContext : context
  val renderContext : context -> string

  val mapTy : Type.ty -> LLVM.ty

  type compiled_exp = LLVM.instruction list * LLVM.operand * LLVM.register_names * LLVM.label_names

  val compileExp : TAST.tast -> LLVM.register_names -> LLVM.label_names -> compiled_exp
  val compileFunc : context -> Function.func -> TAST.tast -> context
end
