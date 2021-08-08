package org.compiler.example
package interpreter

import interpreter.InterpreterResult.{InterResult, unit}
import interpreter.stmt.InterStmt.execute
import parser.stmt.Stmt

class Interpreter {

  def interpreter(stmtList: List[Stmt]): InterResult[Unit] = env => stmtList match {
    case List() =>  unit(Right())(env)
    case h::t   => interpreter(t)(execute(h)(env)._2)
  }
}
