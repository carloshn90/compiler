package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.InterpreterResult.{InterResult, InterResultMonad, unit}
import interpreter.MathExpr._
import interpreter.PrettierOutput.stringify
import lexer._
import parser.expr._
import parser.stmt.{Expression, Print, Stmt, Var}

import cats.implicits._

class Interpreter {

  def interpreter(stmtList: List[Stmt]): InterResult[Unit] = env => stmtList match {
    case List() =>  unit(Right())(env)
    case h::t   => interpreter(t)(execute(h)(env)._2)
  }

  private def visitExpressionStmt(expr: Expr): InterResult[Unit] =
    evaluate(expr).map(_ => {})

  private def visitPrintStmt(expr: Expr): InterResult[Unit] =
    evaluate(expr).map(expr => println(stringify(expr)))

  private def visitBinaryExpr(leftExpr: Expr, token: Token, rightExpr: Expr): InterResult[Any] = {

    val leftValue: InterResult[Any] = evaluate(leftExpr)
    val rightValue: InterResult[Any] = evaluate(rightExpr)
    implicit val line: Int = token.line

    token.tokenType match {
      case MINUS          => mathExpr(leftValue, rightValue)(_ - _)
      case SLASH          => mathExpr(leftValue, rightValue)(_ / _)
      case STAR           => mathExpr(leftValue, rightValue)(_ * _)
      case PLUS           => addExpr(leftValue, rightValue)
      case GREATER        => logicExpr(leftValue, rightValue)(_ > _)
      case GREATER_EQUAL  => logicExpr(leftValue, rightValue)(_ >= _)
      case LESS           => logicExpr(leftValue, rightValue)(_ < _)
      case LESS_EQUAL     => logicExpr(leftValue, rightValue)(_ <= _)
      case BANG_EQUAL     => leftValue.map2(rightValue)(!isEqual(_, _))
      case EQUAL_EQUAL    => leftValue.map2(rightValue)(isEqual)
    }
  }

  private def visitGroupingExpr(expr: Expr): InterResult[Any] = evaluate(expr)

  private def visitLiteralExpr(value: Any): InterResult[Any] = unit(Right(value))

  private def visitUnaryExpr(token: Token, rightExpr: Expr): InterResult[Any] = {

    implicit val line: Int = token.line
    val rightValue = evaluate(rightExpr)

    token.tokenType match {
      case MINUS  => minusExpr(rightValue)
      case BANG   => rightValue.map(isTruthy)
    }
  }

  private def visitVariableExpr(token: Token): InterResult[Any] = env => unit(getValue(env, token))(env)

  private def visitVarStmt(token: Token, expr: Expr): InterResult[Unit] = environment => {
    val (right: Either[ErrorCompiler, Any], env: Environment) = evaluate(expr)(environment)
    right match {
      case Right(null)   => defineVar(token.lexeme, Nil)(env)
      case Right(value)  => defineVar(token.lexeme, value)(env)
      case _             => defineVar(token.lexeme, Nil)(env)
    }
  }

  private def execute(stmt: Stmt): InterResult[Unit] = stmt match {
    case Expression(expr)   => visitExpressionStmt(expr)
    case Print(expr)        => visitPrintStmt(expr)
    case Var(token, expr)   => visitVarStmt(token, expr)
  }

  private def evaluate(expr: Expr): InterResult[Any] = expr match {
    case Binary(left, operator, right)  => visitBinaryExpr(left, operator, right)
    case Grouping(expr)                 => visitGroupingExpr(expr)
    case Literal(value)                 => visitLiteralExpr(value)
    case Unary(token, right)            => visitUnaryExpr(token, right)
    case Variable(token)                => visitVariableExpr(token)
  }

  private def defineVar(name: String, value: Any): InterResult[Unit] = environment =>
    (Right(), environment.define(name, value))

  private def getValue(env: Environment, token: Token): Either[ErrorCompiler, Any] =
    env.get(token)
    .leftMap(err => ErrorCompiler(token.line, err))
}
