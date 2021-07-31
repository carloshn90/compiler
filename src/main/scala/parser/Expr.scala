package org.compiler.example
package parser

import lexer.Token

import error.ErrorCompiler

trait Visitor[R] {
  def visitBinaryExpr(expr: Binary): Either[ErrorCompiler, R]
  def visitGroupingExpr(expr: Grouping): Either[ErrorCompiler, R]
  def visitLiteralExpr(expr: Literal): Either[ErrorCompiler, R]
  def visitUnaryExpr(expr: Unary): Either[ErrorCompiler, R]
}

abstract class Expr {
  def accept[R](visitor: Visitor[R]): Either[ErrorCompiler, R]
}

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr {
  override def accept[R](visitor: Visitor[R]): Either[ErrorCompiler, R] = visitor.visitBinaryExpr(this)
}

case class Grouping(expression: Expr) extends Expr {
  override def accept[R](visitor: Visitor[R]): Either[ErrorCompiler, R] = visitor.visitGroupingExpr(this)
}

case class Literal(value: Any) extends Expr {
  override def accept[R](visitor: Visitor[R]): Either[ErrorCompiler, R] = visitor.visitLiteralExpr(this)
}

case class Unary(token: Token, right: Expr) extends Expr {
  override def accept[R](visitor: Visitor[R]): Either[ErrorCompiler, R] = visitor.visitUnaryExpr(this)
}
