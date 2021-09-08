package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.function.Callable
import interpreter.{InterpreterState, Result}
import lexer.Token
import parser.expr.Expr

object InterCall {

  def interCall(funExpr: Expr, funName: Token, arguments: List[Expr]): InterResult[Result] = for {
    fun   <- evaluate(funExpr)
    args  <- evaluateArguments(arguments)
    call  <- callFunction(fun, funName, args)
  } yield call

  private def evaluateArguments(arguments: List[Expr]): InterResult[List[Result]] =
    arguments.foldRight(unit(Right(List[Result]())))((h: Expr, t: InterResult[List[Result]]) => evaluateArgument(h, t))

  private def evaluateArgument(argument: Expr, arguments: InterResult[List[Result]]): InterResult[List[Result]] =
    evaluate(argument).map2(arguments)((hh: Result, tt: List[Result]) => hh +: tt)

  private def callFunction(function: Result, funName: Token, args: List[Result]): InterResult[Result] = function match {
    case InterpreterState(_, Some(fun: Callable)) if fun.argumentSize != args.size  => unit(Left(ErrorCompiler(funName.line, s"Expected ${fun.argumentSize} arguments but got ${args.size}.")))
    case InterpreterState(prints, Some(fun: Callable))                              => fun.call(funName, args).map(r => InterpreterState(r.printList ::: prints, r.value))
    case _                                                                          => unit(Left(ErrorCompiler(funName.line, "Invalid function type")))
  }
}
