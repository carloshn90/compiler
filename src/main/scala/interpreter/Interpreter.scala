package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.Int.InterResult
import interpreter.MathExpr.{addExpr, logicExpr, mathExpr, isEqual, isTruthy, minusExpr}
import lexer._
import parser._

import cats.implicits._
import interpreter.PrettierOutput.stringify

object Int {
  type InterResult = Either[ErrorCompiler, Any]
}

class Interpreter extends Visitor[Any] {

  def interpreter(expr: Expr): Either[ErrorCompiler, String] =
    evaluate(expr).map(stringify)

  override def visitBinaryExpr(expr: Binary): InterResult = {

    val left = evaluate(expr.left)
    val right = evaluate(expr.right)
    implicit val line: Int = expr.operator.line

    expr.operator.tokenType match {
      case MINUS          => mathExpr(left, right)(_ - _)
      case SLASH          => mathExpr(left, right)(_ / _)
      case STAR           => mathExpr(left, right)(_ * _)
      case PLUS           => addExpr(left, right)
      case GREATER        => logicExpr(left, right)(_ > _)
      case GREATER_EQUAL  => logicExpr(left, right)(_ >= _)
      case LESS           => logicExpr(left, right)(_ < _)
      case LESS_EQUAL     => logicExpr(left, right)(_ <= _)
      case BANG_EQUAL     => left.map2(right)(!isEqual(_, _))
      case EQUAL_EQUAL    => left.map2(right)(isEqual)
    }
  }

  override def visitGroupingExpr(expr: Grouping): InterResult = evaluate(expr.expression)

  override def visitLiteralExpr(expr: Literal): InterResult = Right(expr.value)

  override def visitUnaryExpr(expr: Unary): InterResult = {

    implicit val line: Int = expr.token.line
    val right = evaluate(expr.right)

    expr.token.tokenType match {
      case MINUS  => minusExpr(right)
      case BANG   => right.map(isTruthy)
    }
  }

  private def evaluate(expr: Expr): InterResult = expr.accept(this)
}
