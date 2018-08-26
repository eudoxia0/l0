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

  datatype exp_ast = ConstBool of bool
                   | ConstInt of int
                   | ConstString of string
                   | ConstNull
                   | Var of string
                   | Binop of Binop.binop * exp_ast * exp_ast
                   | Cast of ty * exp_ast
                   | Deref of exp_ast
                   | AddressOf of exp_ast
                   | SizeOf of ty
                   | StructInitializer of string * (string * exp_ast) list
                   | StructAccess of exp_ast * string
                   | Adjacent of exp_ast list
                   | Raw of string

  datatype block_cast = Sequence of block_cast list
                      | Block of block_cast list
                      | Declare of ty * string
                      | Assign of exp_ast * exp_ast
                      | Cond of exp_ast * block_cast * block_cast
                      | While of exp_ast * block_cast
                      | Funcall of string option * string * exp_ast list

  datatype top_cast = FunctionDef of string * param list * ty * block_cast * exp_ast
                    | StructDef of string * slot list
       and param = Param of string * ty
       and slot = Slot of string * ty

  (* Identifiers *)

  fun escapeIdent s = String.concat (map escapeChar (String.explode s))
  and escapeChar #"+" = "_p"
    | escapeChar #"-" = "__"
    | escapeChar #"*" = "_m"
    | escapeChar #"/" = "_d"
    | escapeChar #">" = "_g"
    | escapeChar #"<" = "_l"
    | escapeChar #"=" = "_e"
    | escapeChar #"'" = "_q"
    | escapeChar c = str c

  (* Rendering *)

  local
    open Binop
  in
    fun binopStr Add = "+"
      | binopStr Sub = "-"
      | binopStr Mul = "*"
      | binopStr Div = "/"
      | binopStr Eq = "=="
      | binopStr NEq = "!="
      | binopStr LT = "<"
      | binopStr LEq = "<="
      | binopStr GT = ">"
      | binopStr GEq = ">="
  end

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

  fun renderExp (ConstBool true) = "true"
    | renderExp (ConstBool false) = "false"
    | renderExp (ConstInt i) = (if i < 0 then "-" else "") ^ (Int.toString (abs i))
    | renderExp (ConstString s) =
      let fun tr #"\"" = "\\\""
            | tr c = str c
      in
          "\"" ^ (String.translate tr s) ^ "\""
      end
    | renderExp ConstNull = "NULL"
    | renderExp (Var s) = (escapeIdent s)
    | renderExp (Binop (oper, a, b)) =
      "(" ^ (renderExp a) ^ " " ^ (binopStr oper) ^ " " ^ (renderExp b) ^ ")"
    | renderExp (Cast (ty, a)) = "((" ^ (renderType ty) ^ ")(" ^ (renderExp a) ^ "))"
    | renderExp (Deref e) = "*" ^ (renderExp e)
    | renderExp (AddressOf e) = "&" ^ (renderExp e)
    | renderExp (SizeOf t) = "sizeof(" ^ (renderType t) ^ ")"
    | renderExp (StructInitializer (name, inits)) =
      "(("
      ^ (escapeIdent name)
      ^ ") { "
      ^ (String.concatWith ", " (map (fn (n, e) => "." ^ (escapeIdent n) ^ " = " ^ (renderExp e)) inits))
      ^ " })"
    | renderExp (StructAccess (r, slot)) =
      (renderExp r)
      ^ "."
      ^ (escapeIdent slot)
    | renderExp (Adjacent l) = String.concatWith " " (map renderExp l)
    | renderExp (Raw s) = s

  fun renderBlock' d (Sequence l) = sepBy "\n" (map (renderBlock' d) l)
    | renderBlock' d (Block l) = "{\n" ^ (sepBy "\n" (map (renderBlock' d) l)) ^ "\n" ^ (pad (unindent d)) ^ "}"
    | renderBlock' d (Declare (t, n)) = (pad d) ^ (renderType t) ^ " " ^ (escapeIdent n) ^ ";"
    | renderBlock' d (Assign (var, v)) = (pad d) ^ (renderExp var) ^ " = " ^ (renderExp v) ^ ";"
    | renderBlock' d (Cond (t, c, a)) = (pad d) ^ "if (" ^ (renderExp t) ^ ") " ^ (renderBlock' (indent d) c)
                                         ^ " else " ^ (renderBlock' (indent d) a)
    | renderBlock' d (While (t, b)) =
      (pad d) ^ "while (" ^ (renderExp t) ^ ") {\n" ^ (renderBlock' (indent d) b) ^ "\n" ^ (pad d) ^ "}"
    | renderBlock' d (Funcall (res, f, args)) =
      (pad d) ^ (renderRes res) ^ (escapeIdent f) ^ "(" ^ (sepBy "," (map renderExp args)) ^ ");"
  and renderRes (SOME res) = (escapeIdent res) ^ " = "
    | renderRes NONE = ""

  fun renderBlock b = renderBlock' (indent 0) b

  fun renderTop (FunctionDef (name, params, rt, body, retval)) =
      let val params' = sepBy "," (map renderParam params)
          and rt' = renderType rt
          and name' = escapeIdent name
          and body' = (renderBlock body) ^ "\n  return " ^ (renderExp retval) ^ ";"
      in
          rt' ^ " " ^ name' ^ "(" ^ params' ^ ") {\n" ^ body' ^ "\n}"
      end
    | renderTop (StructDef (name, slots)) =
      let val name' = escapeIdent name
          and slots' = String.concatWith " " (map renderSlot slots)
      in
          "typedef struct { " ^ slots' ^ " } " ^ name' ^ ";\n"
      end

  and renderParam (Param (n, t)) =
      (renderType t) ^ " " ^ n
  and renderSlot (Slot (n, t)) =
      (renderType t) ^ " " ^ (escapeIdent n) ^ ";"
end
