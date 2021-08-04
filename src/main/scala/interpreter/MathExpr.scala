package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.InterpreterResult.{unit, InterResult, InterResultMonad}

object MathExpr {

  def mathExpr(left: InterResult[Any], right: InterResult[Any])
              (f: (Double, Double) => Double)(implicit line: Int): InterResult[Any] =
    calculateBinaryExpr[Double, Double](left, right)(f)

  def logicExpr(left: InterResult[Any], right: InterResult[Any])
               (f: (Double, Double) => Boolean)(implicit line: Int): InterResult[Any] =
    calculateBinaryExpr[Double, Boolean](left, right)(f)

  def addExpr(left: InterResult[Any], right: InterResult[Any])(implicit line: Int): InterResult[Any] = for {
    l <- left
    r <- right
    lPlusR <- add(l, r)
  } yield lPlusR

  def minusExpr(expr: InterResult[Any])(implicit line: Int): InterResult[Any] = for{
    r <- expr
    rd <- toConvert[Double](r)
  } yield -rd

  def isEqual(left: Any, right: Any): Boolean = (left, right) match {
    case (Nil, Nil) => true
    case (Nil, _)   => false
    case _          => left.equals(right)
  }

  def isTruthy(value: Any): Boolean = value match {
    case Nil            => false
    case value: Boolean => value
    case _              => true
  }

  private def add(left: Any, right: Any)(implicit line: Int): InterResult[Any] = (left, right) match {
    case (l: String, r: String) => unit(Right(l + r))
    case (l: Double, r: Double) => unit(Right(l + r))
    case _                      => unit(Left(ErrorCompiler(line, s"It isn't possible to add these values: $left + $right")))
  }

  private def calculateBinaryExpr[A, B](left: InterResult[Any], right: InterResult[Any])(f: (A, A) => B)
                                       (implicit line: Int): InterResult[Any] = for {
    l   <- left
    r   <- right
    ld  <- toConvert[A](l)
    rd  <- toConvert[A](r)
  } yield f(ld, rd)

  private def toConvert[A](value: Any)(implicit line: Int): InterResult[A] = value match {
    case d: A   => unit(Right(d))
    case _      => unit(Left(ErrorCompiler(line, s"Impossible to convert $value to double")))
  }
}
