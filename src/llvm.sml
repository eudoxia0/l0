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

structure LLVM :> LLVM = struct
  datatype ty = Int of bit_width
              | Float of float_width
              | Pointer of ty
              | FuncPointer of ty * ty list
              | Struct of ty list
       and bit_width = Word1 | Word8 | Word16 | Word32 | Word64
       and float_width = Single | Double

  fun renderType (Int Word1) = "i1"
    | renderType (Int Word8) = "i8"
    | renderType (Int Word16) = "i16"
    | renderType (Int Word32) = "i32"
    | renderType (Int Word64) = "i64"
    | renderType (Float Single) = "float"
    | renderType (Float Double) = "double"
    | renderType (Pointer t) = (renderType t) ^ "*"
    | renderType (FuncPointer (rt, ts)) =
      let val rt' = renderType rt
          and ts' = String.concatWith " " (map renderType ts)
      in
          rt' ^ " (" ^ ts' ^ ")*"
      end
    | renderType (Struct ts) = "{" ^ (String.concatWith ", " (map renderType ts)) ^ "}"
end
