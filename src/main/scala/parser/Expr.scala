package org.compiler.example
package parser

import lexer.Token

trait Expr

case class Binary(left: Expr, operator: Token, right: Expr) extends Expr
case class Grouping(expression: Expr) extends Expr
case class Literal(value: Any) extends Expr
case class Unary(token: Token, right: Expr) extends Expr
case class Variable(token: Token) extends Expr
