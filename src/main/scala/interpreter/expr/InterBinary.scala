package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.Converter.convertToDouble
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import lexer.{BANG_EQUAL, EQUAL_EQUAL, GREATER, GREATER_EQUAL, LESS, LESS_EQUAL, MINUS, PLUS, SLASH, STAR, Token}
import parser.expr.Expr

object InterBinary {

  def interBinary(leftExpr: Expr, token: Token, rightExpr: Expr): InterResult[Any] = {

    val leftValue: InterResult[Any] = evaluate(leftExpr)
    val rightValue: InterResult[Any] = evaluate(rightExpr)
    implicit val line: Int = token.line

    token.tokenType match {
      case PLUS           => add(leftValue, rightValue)
      case SLASH          => executeOperation(leftValue, rightValue)(_ / _)
      case STAR           => executeOperation(leftValue, rightValue)(_ * _)
      case MINUS          => executeOperation(leftValue, rightValue)(_ - _)
      case GREATER        => executeLogic(leftValue, rightValue)(_ > _)
      case GREATER_EQUAL  => executeLogic(leftValue, rightValue)(_ >= _)
      case LESS           => executeLogic(leftValue, rightValue)(_ < _)
      case LESS_EQUAL     => executeLogic(leftValue, rightValue)(_ <= _)
      case BANG_EQUAL     => leftValue.map2(rightValue)(!isEqual(_, _))
      case EQUAL_EQUAL    => leftValue.map2(rightValue)(isEqual)
    }
  }

  private def add(left: InterResult[Any], right: InterResult[Any])(implicit line: Int): InterResult[Any] = for {
    l <- left
    r <- right
    lPlusR <- addValues(l, r)
  } yield lPlusR

  private def addValues(left: Any, right: Any)(implicit line: Int): InterResult[Any] = (left, right) match {
    case (l: String, r: String) => unit(Right(l + r))
    case (l: Double, r: Double) => unit(Right(l + r))
    case _                      => unit(Left(ErrorCompiler(line, s"It isn't possible to add these values: $left + $right")))
  }

  private def executeOperation(left: InterResult[Any], right: InterResult[Any])
              (f: (Double, Double) => Double)(implicit line: Int): InterResult[Any] =for {
    l   <- left
    r   <- right
    ld  <- convertToDouble(l)
    rd  <- convertToDouble(r)
  } yield f(ld, rd)

  private def executeLogic(left: InterResult[Any], right: InterResult[Any])
               (f: (Double, Double) => Boolean)(implicit line: Int): InterResult[Any] = for {
    l   <- left
    r   <- right
    ld  <- convertToDouble(l)
    rd  <- convertToDouble(r)
  } yield f(ld, rd)

  private def isEqual(left: Any, right: Any): Boolean = (left, right) match {
    case (Nil, Nil) => true
    case (Nil, _)   => false
    case _          => left.equals(right)
  }
}
