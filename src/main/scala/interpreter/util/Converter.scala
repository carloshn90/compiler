package org.compiler.example
package interpreter.util

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, unit}

object Converter {

  def convertToDouble(value: Any)(implicit line: Int): InterResult[Double] =
    value match {
      case d: Double => unit(Right(d))
      case i: Int => unit(Right(i.toDouble))
      case s: String => unit(convertStringToDouble(s))
      case _ => unit(Left(ErrorCompiler(line, s"Impossible to convert $value to double")))
    }

  def convertToBoolean(value: Any)(implicit line: Int): InterResult[Boolean] =
    value match {
      case d: Boolean => unit(Right(d))
      case s: String => unit(convertStringToBoolean(s))
      case _ => unit(Left(ErrorCompiler(line, s"Impossible to convert $value to boolean")))
    }

  def isTruthy(value: Any): Boolean = value match {
    case Nil            => false
    case value: Boolean => value
    case _              => true
  }

  private def convertStringToDouble(value: String)(implicit line: Int): Either[ErrorCompiler, Double] = try {
    Right(value.toDouble)
  } catch {
    case _: Throwable => Left(ErrorCompiler(line, s"Impossible to convert $value to double"))
  }

  private def convertStringToBoolean(value: String)(implicit line: Int): Either[ErrorCompiler, Boolean] = try {
    Right(value.toBoolean)
  } catch {
    case _: Throwable => Left(ErrorCompiler(line, s"Impossible to convert $value to boolean"))
  }
}
