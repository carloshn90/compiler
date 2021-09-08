package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, unit}
import interpreter.stmt.InterStmt.execute
import parser.stmt.Stmt

import cats.implicits.catsSyntaxApply

class Interpreter {

  def interpreter(stmtList: List[Stmt], carryResult: Either[ErrorCompiler, List[String]]): InterResult[List[String]] = env => stmtList match {
    case List() =>  unit(carryResult)(env)
    case h::t   =>
      val (result, e) = execute(h)(env)
      interpreter(t, result.map2(carryResult)((r, carry) => carry:::r.printList))(e)
  }
}
