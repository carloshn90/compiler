package org.compiler.example
package interpreter.stmt

import interpreter.InterResult.{InterResult, InterResultMonad}
import interpreter.stmt.InterBlock.interBlock
import interpreter.stmt.InterClass.interClass
import interpreter.stmt.InterExpression.interExpression
import interpreter.stmt.InterFunction.interFunction
import interpreter.stmt.InterIf.interIf
import interpreter.stmt.InterPrint.interPrint
import interpreter.stmt.InterReturn.interReturn
import interpreter.stmt.InterVar.interVar
import interpreter.stmt.InterWhile.interWhile
import interpreter.{InterpreterState, Result}
import parser.stmt._

object InterStmt {

  def execute(stmt: Stmt): InterResult[Result] = stmt match {
    case Block(statements)                      => interBlock(statements)
    case Class(name, vars, methods)             => interClass(name, vars, methods)
    case Expression(expr)                       => interExpression(expr)
    case Return(_, value)                       => interReturn(value)
    case Function(name, params, body)           => interFunction(name, params, body)
    case If(condition, thenBranch, elseBranch)  => interIf(condition, thenBranch, elseBranch)
    case Print(expr)                            => interPrint(expr)
    case Var(token, expr)                       => interVar(token, expr).map(transformOptionToList)
    case While(condition, body)                 => interWhile(condition, body)
  }

  private def transformOptionToList(option: Option[String]): Result = option match {
    case Some(value) => InterpreterState(List(value), None)
    case _           => InterpreterState(List(), None)
  }


}
