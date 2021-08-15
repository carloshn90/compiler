package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad}
import interpreter.stmt.InterBlock.interBlock
import interpreter.stmt.InterExpression.interExpression
import interpreter.stmt.InterIf.interIf
import interpreter.stmt.InterPrint.interPrint
import interpreter.stmt.InterVar.interVar
import parser.stmt.{Block, Expression, If, Print, Stmt, Var}

object InterStmt {

  def execute(stmt: Stmt): InterResult[List[String]] = env => stmt match {
    case Block(statements)                      => interBlock(statements, env)(env)
    case Expression(expr)                       => interExpression(expr).map(transformOptionToList)(env)
    case If(condition, thenBranch, elseBranch)  => interIf(condition, thenBranch, elseBranch)(env)
    case Print(expr)                            => interPrint(expr).map(transformOptionToList)(env)
    case Var(token, expr)                       => interVar(token, expr).map(transformOptionToList)(env)
  }

  private def transformOptionToList(option: Option[String]): List[String] = option match {
    case Some(value) => List(value)
    case _           => List()
  }


}
