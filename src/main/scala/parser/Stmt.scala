package org.compiler.example
package parser

import lexer.Token

sealed trait Stmt
case class Expression(expression: Expr) extends Stmt
case class Print(expression: Expr) extends Stmt
case class Var(token: Token, initializer: Expr) extends Stmt
