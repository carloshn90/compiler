package org.compiler.example
package interpreter.expr

import interpreter.InterResult.{InterResult, unit}
import interpreter.{InterpreterState, Result}

object InterLiteral {

  def interLiteral(value: Any): InterResult[Result] = unit(Right(InterpreterState(List(), Some(value))))
}
