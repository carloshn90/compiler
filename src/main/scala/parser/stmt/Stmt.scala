package org.compiler.example
package parser.stmt

import lexer.Token
import parser.expr.Expr
import parser.grammar.Grammar

sealed trait Stmt extends Grammar

object Stmt {
  case object None extends Stmt
}

case class Block(statements: List[Stmt]) extends Stmt
case class Class(name: Token, variables: List[Var], methods: List[Function]) extends Stmt
case class Expression(expr: Expr) extends Stmt
case class Function(name: Token, params: List[Token], body: List[Stmt]) extends Stmt
case class If(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt]) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Return(keyword: Token, value: Expr) extends Stmt
case class Var(token: Token, initializer: Expr) extends Stmt
case class While(condition: Expr, body: Stmt) extends Stmt
