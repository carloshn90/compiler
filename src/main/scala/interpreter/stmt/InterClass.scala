package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, define}
import interpreter.`class`.InterClass
import interpreter.function.InterFunction
import interpreter.{Environment, Result}
import lexer.Token
import parser.stmt.Function

object InterClass {

  def interClass(name: Token, methods: List[Function]): InterResult[Result] = env => {
    val interClass: InterClass = new InterClass(name.lexeme, defineMethods(methods, env))
    define(name, interClass)(env)
  }

  private def defineMethods(methods: List[Function], env: Environment): Map[String, Any] = methods match {
    case List() => Map()
    case h::t   => Map(h.name.lexeme -> new InterFunction(h, env)) ++ defineMethods(t, env)
  }
}
