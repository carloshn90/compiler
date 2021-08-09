package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.InterpreterResult.{InterResult, unit}

object Converter {

  def convert[A](value: Any)(implicit line: Int): InterResult[A] = value match {
    case d: A => unit(Right(d))
    case _    => unit(Left(ErrorCompiler(line, s"Impossible to convert $value to double")))
  }
}
