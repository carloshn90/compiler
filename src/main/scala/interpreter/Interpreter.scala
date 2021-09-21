package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, unit, InterResultMonad}
import interpreter.stmt.InterStmt.execute
import parser.stmt.Stmt

class Interpreter {

  def interpreter(stmtList: List[Stmt], carryResult: Either[ErrorCompiler, List[String]]): InterResult[List[String]] = env => stmtList match {
    case List() =>  unit(carryResult)(env)
    case h::t   => execute(h).flatMap(r => interpreter(t, carryResult.map(carry => carry ::: r.printList)))(env)
  }
}
