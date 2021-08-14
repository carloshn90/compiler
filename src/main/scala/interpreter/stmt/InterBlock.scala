package org.compiler.example
package interpreter.stmt

import interpreter.Environment
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.stmt.InterStmt.execute
import parser.stmt.Stmt

object InterBlock {

  def interBlock(block: List[Stmt], originalEnv: Environment): InterResult[List[String]] = env => block match {
    case List() => unit(Right(List()))(originalEnv)
    case h::t   => executeStmt(h, t, originalEnv)(env)
  }

   private def executeStmt(stmt: Stmt, block: List[Stmt], originalEnv: Environment): InterResult[List[String]] =
     execute(stmt)
       .flatMap(value => interBlock(block, originalEnv)
       .map(rr => value ::: rr))
}
