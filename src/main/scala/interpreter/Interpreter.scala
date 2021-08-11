package org.compiler.example
package interpreter

import error.ErrorCompiler
import interpreter.InterpreterResult.{InterResult, unit}
import interpreter.stmt.InterStmt.execute
import parser.stmt.Stmt

import cats.implicits.catsSyntaxApply

class Interpreter {

  def interpreter(stmtList: List[Stmt], result: Either[ErrorCompiler, List[String]]): InterResult[List[String]] = env => stmtList match {
    case List() =>  unit(result)(env)
    case h::t   =>
      val (r, e) = execute(h)(env)
      interpreter(t, result.map2(r)(addToList))(e)
  }

  private def addToList(list: List[String], value: Option[String]): List[String] =
    value.map(v => v :: list).getOrElse(list)
}
