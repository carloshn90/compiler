package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.stmt.InterStmt.execute
import interpreter.{Environment, InterpreterState, Result, ReturnResult}
import parser.stmt.Stmt

object InterBlock {

  def interBlock(block: List[Stmt]): InterResult[Result] = env =>
    interAllStatements(block)(new Environment(Some(env)))

  private def interAllStatements(block: List[Stmt], acc: Result = InterpreterState(List(), None)): InterResult[Result] = env => block match {
    case List() => unit(Right(acc))(env.restore())
    case h::t   => executeStmt(h, t, acc)(env)
  }

   private def executeStmt(stmt: Stmt, block: List[Stmt], acc: Result): InterResult[Result] =
   execute(stmt).flatMap(result => checkReturnResult(result, block, acc))

  private def checkReturnResult(result: Result, block: List[Stmt], acc: Result): InterResult[Result] =  result match {
    case ReturnResult(printList, value)     => interAllStatements(List(), InterpreterState(acc.printList ::: printList, value))
    case InterpreterState(printList, value) => interAllStatements(block, InterpreterState(acc.printList ::: printList, value))
    case _                                  => unit(Left(ErrorCompiler(-1, "Unexpected error")))
  }
}
