package org.compiler.example
package interpreter.stmt

import interpreter.Environment
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.stmt.InterStmt.execute
import parser.stmt.Stmt

object InterBlock {

  def interBlock(block: List[Stmt]): InterResult[List[String]] = env =>
    interAllStatements(block)(new Environment(Some(env)))

  private def interAllStatements(block: List[Stmt]): InterResult[List[String]] = env => block match {
    case List() => unit(Right(List()))(env.restore())
    case h::t   => executeStmt(h, t)(env)
  }

   private def executeStmt(stmt: Stmt, block: List[Stmt]): InterResult[List[String]] =
     execute(stmt)
       .flatMap(value => interAllStatements(block)
       .map(rr => value ::: rr))
}
