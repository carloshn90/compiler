package org.compiler.example
package interpreter.`class`

import interpreter.InterResult.{InterResult, unit}
import interpreter.function.Callable
import interpreter.{Result, ReturnResult}
import lexer.Token

class InterClass(name: String) extends Callable {

  override def argumentSize: Int = 0

  override def call(funName: Token, arguments: List[Result]): InterResult[Result] =
    unit(Right(ReturnResult(List(), Some(new InterInstance(this)))))

  def getName: String = name

  override def toString = s"InterClass $name"
}
