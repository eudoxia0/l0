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
  datatype compiler = Compiler of Type.tenv * Function.fenv * CppBackend.context

  local
    open Function
    open Type
  in
    val emptyCompiler =
        let val interim_not = Function ("interim_not", [Param (Ident.mkIdent "v" 0, Bool)], Bool)
        in
            Compiler (Type.emptyTenv,
                      Map.iadd Map.empty ("interim_not", interim_not),
                      CppBackend.emptyContext)
        end
  end

  fun compilerTypeEnv (Compiler (t, _, _)) = t

  fun compileAST (Compiler (tenv, fenv, ctx)) ast =
      let val (func, arast) = ARAST.alphaRename ast
      in
          print "ARAST: \n";
          print (ARAST.toString arast);
          let val fenv' = case Map.add fenv (Function.funcName func, func) of
                              SOME fenv' => fenv'
                            | NONE => raise Fail "Function already defined"
              and bindings = Function.funcBindings func
          in
              let val oast = OAST.augment arast
              in
                  let val tast = TAST.augment oast
                                              (TAST.mkContext bindings
                                                              tenv
                                                              fenv')
                  in
                      if (TAST.typeOf tast) <> Function.funcRT func then
                          raise Fail "Return type does not match type of body"
                      else
                          let val ctx' = CppBackend.defineFunction ctx func tast
                          in
                              Compiler (tenv, fenv', ctx')
                          end
                  end
              end
          end
      end

  fun compileString c s =
    let val (sexp, _) = Parser.parseString s
    in
        compileAST c (AST.parseToplevel sexp (compilerTypeEnv c))
    end

  fun compileForms c (form::rest) =
      let val c' = compileAST c (AST.parseToplevel form (compilerTypeEnv c))
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

  fun compilerCode (Compiler (_, _, ctx)) =
      CppBackend.renderContext ctx
end
