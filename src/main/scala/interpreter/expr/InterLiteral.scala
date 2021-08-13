package org.compiler.example
package interpreter.expr

import interpreter.InterResult.{InterResult, unit}

object InterLiteral {

  def interLiteral(value: Any): InterResult[Any] = unit(Right(value))
}
