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

structure Compiler :> COMPILER = struct
  open SymTab

  datatype compiler = Compiler of Type.tenv * Function.fenv * LLVM.context

  local
    open Function
    open Type
  in
    val emptyCompiler =
        let val interim_not = Function ("interim_not", [Param ("v", Bool)], Bool)
        in
            Compiler (empty, bind ("interim_not", interim_not) empty, LLVM.emptyContext)
        end
  end

  fun compilerTypeEnv (Compiler (t, _, _)) = t

  fun compileAST (Compiler (tenv, fenv, llvmCtx)) ast =
    (case ast of
         (AST.Defun (func, ast)) =>
         let val fenv' = bind (Function.funcName func, func) fenv
             and (stack, namegen) = Function.toStack func
         in
             let val oast = OAST.augment (ARAST.alphaRename ast namegen)
             in
                 let val tast = TAST.augment oast
                                             (TAST.mkContext stack
                                                             tenv
                                                             fenv')
                 in
                     if (TAST.typeOf tast) <> Function.funcRT func then
                         raise Fail "Return type does not match type of body"
                     else
                         let
                         in
                             print (";; Define function " ^ (Function.funcName func) ^ "\n");
                             print "Context:\n";
                             print (LLVM.renderContext llvmCtx)
                             print "\n"
                             Compiler (tenv, fenv', llvmCtx)
                         end
                 end
             end
         end)

  fun compileString c s =
    let val (sexp, _) = Parser.parseString s
    in
        compileAST c (AST.parseToplevel sexp (compilerTypeEnv c))
    end

  fun compileForms c (form::rest) = let val c' = compileAST c (AST.parseToplevel form (compilerTypeEnv c))
                                    in
                                        compileForms c' rest
                                    end
    | compileForms c nil = c

  fun compileFile c path =
    let val code = "(" ^ (Util.readFileToString path) ^ ")"
    in
        case (Parser.parseString code) of
            (Parser.List l, _) => compileForms c l
          | _ => raise Fail "Impossible"
    end
end
