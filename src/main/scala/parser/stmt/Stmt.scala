package org.compiler.example
package parser.stmt

import lexer.Token
import parser.expr.Expr

import org.compiler.example.parser.grammar.Grammar

sealed trait Stmt extends Grammar

case class Expression(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class Var(token: Token, initializer: Expr) extends Stmt
