package org.compiler.example
package interpreter

import interpreter.Int.InterResult

import error.ErrorCompiler

object MathExpr {

  def mathExpr(left: InterResult, right: InterResult)
              (f: (Double, Double) => Double)(implicit line: Int): InterResult =
    calculateBinaryExpr[Double, Double](left, right)(f)

  def logicExpr(left: InterResult, right: InterResult)
               (f: (Double, Double) => Boolean)(implicit line: Int): InterResult =
    calculateBinaryExpr[Double, Boolean](left, right)(f)

  def addExpr(left: InterResult, right: InterResult)(implicit line: Int): InterResult = for {
    l <- left
    r <- right
    lPlusR <- add(l, r)
  } yield lPlusR

  def minusExpr(expr: InterResult)(implicit line: Int): InterResult = for{
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

  private def add(left: Any, right: Any)(implicit line: Int): InterResult = (left, right) match {
    case (l: String, r: String) => Right(l + r)
    case (l: Double, r: Double) => Right(l + r)
    case _                      => Left(ErrorCompiler(line, s"It isn't possible to add these values: $left + $right"))
  }

  private def calculateBinaryExpr[A, B](left: InterResult, right: InterResult)(f: (A, A) => B)
                                    (implicit line: Int): InterResult = for {
    l   <- left
    r   <- right
    ld  <- toConvert[A](l)
    rd  <- toConvert[A](r)
  } yield f(ld, rd)

  private def toConvert[A](value: Any)(implicit line: Int): Either[ErrorCompiler, A] = value match {
    case d: A   => Right(d)
    case _      => Left(ErrorCompiler(line, s"Impossible to convert $value to double"))
  }
}
