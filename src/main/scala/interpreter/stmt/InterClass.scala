package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, define, unit}
import interpreter.`class`.InterClass
import interpreter.function.InterFunction
import interpreter.stmt.InterStmt.execute
import interpreter.{Environment, Result}
import lexer.Token
import parser.stmt.{Function, Var}

object InterClass {

  def interClass(name: Token, vars: List[Var], methods: List[Function]): InterResult[Result] = env => {
    addAllFunctionEnv(methods, env) match {
      case Right(e)   => defineClass(name, new InterClass(name.lexeme, defineMethods(methods, e)), vars)(env)
      case Left(err)  => unit(Left(err))(env)
    }
  }

  private def defineVariables(vars: List[Var], env: Environment): Either[ErrorCompiler, Environment] = vars match {
    case List() => Right(env)
    case h::t   =>
      val (result: Either[ErrorCompiler, Result], envResult: Environment) = execute(h)(env)
      result.flatMap(_ => defineVariables(t, envResult))
  }

  private def defineMethods(methods: List[Function], env: Environment): Map[String, InterFunction] = methods match {
    case List() => Map()
    case h::t   => Map(h.name.lexeme -> new InterFunction(h, env)) ++ defineMethods(t, env)
  }

  private def defineClass(name: Token, interClass: InterClass, vars: List[Var]): InterResult[Result] = env => {
    defineVariables(vars, env) match {
      case Right(varEnv) => define(name, interClass)(varEnv)
      case Left(err)     => unit(Left(err))(env)
    }
  }

  private def addAllFunctionEnv(methods: List[Function], env: Environment): Either[ErrorCompiler, Environment] = methods match {
    case List() => Right(env)
    case h::t   => env.define(h.name, new InterFunction(h, env)).flatMap(e => addAllFunctionEnv(t, e))
  }
}
