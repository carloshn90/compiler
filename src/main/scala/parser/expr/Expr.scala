package org.compiler.example
package parser.expr

import lexer.Token
import parser.grammar.Grammar

sealed trait Expr extends Grammar

object Expr {
  case object None extends Expr
}

case class Assign(token: Token, expr: Expr) extends Expr
case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Call(funExpr: Expr, funName: Token, arguments: List[Expr]) extends Expr
case class Grouping(expr: Expr) extends Expr
case class Logical(left: Expr, token: Token, right: Expr) extends Expr
case class Literal(value: Any) extends Expr
case class Unary(token: Token, expr: Expr) extends Expr
case class Variable(token: Token) extends Expr
