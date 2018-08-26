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

structure Type : TYPE = struct
  open SymTab

  datatype ty = Unit
              | Bool
              | Int of signedness * bit_width
              | Str
              | RawPointer of ty
              | Tuple of ty list
       and signedness = Signed | Unsigned
       and bit_width = Word8 | Word16 | Word32 | Word64
       and slot = Slot of string * ty

  fun isEquatable (Tuple _) = false
    | isEquatable (RawPointer _) = false
    | isEquatable _ = true

  fun isNumeric (Int _) = true
    | isNumeric _ = false

  fun isPrintable (Tuple _) = false
    | isPrintable _ = true

  fun tyToString Unit = "unit"
    | tyToString Bool = "bool"
    | tyToString (Int (s, w)) = (signednessStr s) ^ (widthStr w)
    | tyToString Str = "str"
    | tyToString (RawPointer t) = "(rawptr " ^ (tyToString t) ^ ")"
    | tyToString (Tuple l) = "(tuple " ^ (String.concatWith " " (map tyToString l)) ^ ")"
  and signednessStr Signed = "i"
    | signednessStr Unsigned = "u"
  and widthStr Word8 = "8"
    | widthStr Word16 = "16"
    | widthStr Word32 = "32"
    | widthStr Word64 = "64"

  type tenv = ty symtab

  local
    open Parser
  in
    fun parseTypeSpecifier (Symbol "unit") _ = Unit
      | parseTypeSpecifier (Symbol "bool") _ = Bool
      | parseTypeSpecifier (Symbol "u8") _ = Int (Unsigned, Word8)
      | parseTypeSpecifier (Symbol "i8") _ = Int (Signed, Word8)
      | parseTypeSpecifier (Symbol "u16") _ = Int (Unsigned, Word16)
      | parseTypeSpecifier (Symbol "i16") _ = Int (Signed, Word16)
      | parseTypeSpecifier (Symbol "u32") _ = Int (Unsigned, Word32)
      | parseTypeSpecifier (Symbol "i32") _ = Int (Signed, Word32)
      | parseTypeSpecifier (Symbol "u64") _ = Int (Unsigned, Word64)
      | parseTypeSpecifier (Symbol "i64") _ = Int (Signed, Word64)
      | parseTypeSpecifier (Symbol "str") _ = Str
      | parseTypeSpecifier (List [Symbol "rawptr", t]) e = RawPointer (parseTypeSpecifier t e)
      | parseTypeSpecifier (List (Symbol "tuple" :: rest)) e = Tuple (map (fn s => parseTypeSpecifier s e) rest)
      | parseTypeSpecifier (Symbol s) e = lookup s e
      | parseTypeSpecifier _ _ = raise Fail "Bad type specifier"
  end
end
