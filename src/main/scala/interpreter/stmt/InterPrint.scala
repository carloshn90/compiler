package org.compiler.example
package interpreter.stmt

import interpreter.InterpreterResult.{InterResult, InterResultMonad}
import interpreter.PrettierOutput.stringify
import interpreter.expr.InterExpr.evaluate
import parser.expr.Expr

object InterPrint {

  def interPrint(expr: Expr): InterResult[Unit] = evaluate(expr).map(expr => println(stringify(expr)))
}
