package org.compiler.example
package interpreter.`class`

import interpreter.InterResult.{InterResult, unit}
import interpreter.function.{Callable, InterFunction}
import interpreter.{InterpreterState, Result}
import lexer.Token

class InterClass(name: String, methods: Map[String, InterFunction]) extends Callable {

  override def argumentSize: Int = 0

  override def call(funName: Token, arguments: List[Result]): InterResult[Result] =
    unit(Right(InterpreterState(List(), Some(new InterInstance(methods)))))

  def getName: String = name

  def getMethods: Map[String, Any] = methods

  override def toString = s"InterClass $name"
}
