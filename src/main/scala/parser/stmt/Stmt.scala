package org.compiler.example
package parser.stmt

import lexer.Token
import parser.expr.Expr
import parser.grammar.Grammar

sealed trait Stmt extends Grammar

case class Block(statements: List[Stmt]) extends Stmt
case class Expression(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Var(token: Token, initializer: Expr) extends Stmt
