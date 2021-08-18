package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad}
import interpreter.stmt.InterBlock.interBlock
import interpreter.stmt.InterExpression.interExpression
import interpreter.stmt.InterIf.interIf
import interpreter.stmt.InterPrint.interPrint
import interpreter.stmt.InterVar.interVar
import interpreter.stmt.InterWhile.interWhile
import parser.stmt.{Block, Expression, If, Print, Stmt, Var, While}

object InterStmt {

  def execute(stmt: Stmt): InterResult[List[String]] = stmt match {
    case Block(statements)                      => interBlock(statements)
    case Expression(expr)                       => interExpression(expr).map(transformOptionToList)
    case If(condition, thenBranch, elseBranch)  => interIf(condition, thenBranch, elseBranch)
    case Print(expr)                            => interPrint(expr).map(transformOptionToList)
    case Var(token, expr)                       => interVar(token, expr).map(transformOptionToList)
    case While(condition, body)                 => interWhile(condition, body)
  }

  private def transformOptionToList(option: Option[String]): List[String] = option match {
    case Some(value) => List(value)
    case _           => List()
  }


}
