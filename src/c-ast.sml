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

structure CAst :> C_AST = struct
  (* Types *)

  datatype ty = Bool
              | UInt8
              | Int8
              | UInt16
              | Int16
              | UInt32
              | Int32
              | UInt64
              | Int64
              | Pointer of ty
              | Struct of string

  datatype exp_cast = ConstBool of bool
                    | ConstInt of int
                    | ConstString of string
                    | ConstNull
                    | Var of string
                    | Binop of Binop.binop * exp_cast * exp_cast
                    | Cast of ty * exp_cast
                    | Deref of exp_cast
                    | AddressOf of exp_cast
                    | SizeOf of ty
                    | StructInitializer of string * (string * exp_cast) list
                    | StructAccess of exp_cast * string
                    | Adjacent of exp_cast list
                    | Raw of string

  datatype block_cast = Sequence of block_cast list
                      | Block of block_cast list
                      | Declare of ty * string
                      | Assign of exp_cast * exp_cast
                      | Cond of exp_cast * block_cast * block_cast
                      | While of exp_cast * block_cast
                      | Funcall of string option * string * exp_cast list

  datatype top_cast = FunctionDef of string * param list * ty * block_cast * exp_cast
                    | StructDef of string * slot list
       and param = Param of string * ty
       and slot = Slot of string * ty

  (* Rendering *)

  fun renderType Bool = "bool"
    | renderType UInt8 = "uint8_t"
    | renderType Int8 = "int8_t"
    | renderType UInt16 = "uint16_t"
    | renderType Int16 = "int16_t"
    | renderType UInt32 = "uint32_t"
    | renderType Int32 = "int32_t"
    | renderType UInt64 = "uint64_t"
    | renderType Int64 = "int64_t"
    | renderType (Pointer t) = (renderType t) ^ "*"
    | renderType (Struct n) = n

  local
    open Substring
  in
    fun sepBy sep strings = trimWhitespace (String.concatWith sep strings)
    and trimWhitespace s = string (dropl (fn c => c = #"\n") (full s))
  end

  fun pad n =
    if n > 0 then
        " " ^ (pad (n-1))
    else
        ""

  val indentation = 2
  fun indent d = d + indentation
  fun unindent d = d - indentation

  fun renderExp (CConstBool true) = "true"
    | renderExp (CConstBool false) = "false"
    | renderExp (CConstInt i) = (if i < 0 then "-" else "") ^ (Int.toString (abs i))
    | renderExp (CConstString s) =
      let fun tr #"\"" = "\\\""
            | tr c = str c
      in
          "\"" ^ (String.translate tr s) ^ "\""
      end
    | renderExp CConstNull = "NULL"
    | renderExp (CVar s) = (escapeIdent s)
    | renderExp (CBinop (oper, a, b)) =
      "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
    | renderExp (CCast (ty, a)) = "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
    | renderExp (CDeref e) = "*" ^ (renderExp e)
    | renderExp (CAddressOf e) = "&" ^ (renderExp e)
    | renderExp (CSizeOf t) = "sizeof(" ^ (renderType t) ^ ")"
    | renderExp (CStructInitializer (name, inits)) =
      "(("
      ^ (escapeIdent name)
      ^ ") { "
      ^ (String.concatWith ", " (map (fn (n, e) => "." ^ (escapeIdent n) ^ " = " ^ (renderExp e)) inits))
      ^ " })"
    | renderExp (CStructAccess (r, slot)) =
      (renderExp r)
      ^ "."
      ^ (escapeIdent slot)
    | renderExp (CAdjacent l) = String.concatWith " " (map renderExp l)
    | renderExp (CRaw s) = s

  fun renderBlock' d (CSeq l) = sepBy "\n" (map (renderBlock' d) l)
    | renderBlock' d (CBlock l) = "{\n" ^ (sepBy "\n" (map (renderBlock' d) l)) ^ "\n" ^ (pad (unindent d)) ^ "}"
    | renderBlock' d (CDeclare (t, n)) = (pad d) ^ (renderType t) ^ " " ^ (escapeIdent n) ^ ";"
    | renderBlock' d (CAssign (var, v)) = (pad d) ^ (renderExp var) ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock' d (CCond (t, c, a)) = (pad d) ^ "if (" ^ (renderExp t) ^ ") " ^ (renderBlock' (indent d) c)
                                         ^ " else " ^ (renderBlock' (indent d) a)
    | renderBlock' d (CWhile (t, b)) =
      (pad d) ^ "while (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock' (indent d) b) ^ "\n" ^ (pad d) ^ "}"
    | renderBlock' d (CFuncall (res, f, args)) =
      (pad d) ^ (renderRes res) ^ (escapeIdent f) ^ "(" ^ (sepBy "," (map renderExp args)) ^ ");"
  and renderRes (SOME res) = (escapeIdent res) ^ " = "
    | renderRes NONE = ""

  fun renderBlock b = renderBlock' (indent 0) b

  fun renderTop (CFunction (name, params, rt, body, retval)) =
    (renderType rt) ^ " " ^ (escapeIdent name) ^ "(" ^ (sepBy "," (map renderParam params)) ^ ") {\n" ^ (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";\n}"
    | renderTop (CStructDef (name, slots)) =
      "typedef struct { "
      ^ (String.concatWith " " (map (fn (n, t) => (renderType t) ^ " " ^ (escapeIdent n) ^ ";") slots))
      ^ " } " ^ (escapeIdent name) ^ ";\n"
  and renderParam (CParam (n, t)) = (renderType t) ^ " " ^ n
end
