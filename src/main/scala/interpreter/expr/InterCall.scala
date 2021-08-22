package org.compiler.example
package interpreter.expr

import error.ErrorCompiler
import interpreter.InterResult.{InterResult, InterResultMonad, unit}
import interpreter.expr.InterExpr.evaluate
import interpreter.function.Callable
import lexer.Token
import parser.expr.Expr

object InterCall {

  def interCall(funExpr: Expr, token: Token, arguments: List[Expr]): InterResult[Any] = for {
    fun   <- evaluate(funExpr)
    args  <- evaluateArguments(arguments)
    call  <- callFunction(fun, token, args)
  } yield call

  private def evaluateArguments(arguments: List[Expr]): InterResult[List[Any]] =
    arguments.foldRight(unit(Right(List[Any]())))((h: Expr, t: InterResult[List[Any]]) => evaluateArgument(h, t))

  private def evaluateArgument(argument: Expr, arguments: InterResult[List[Any]]): InterResult[List[Any]] =
    evaluate(argument).map2(arguments)((hh: Any, tt: List[Any]) => hh +: tt)

  private def callFunction(function: Any, token: Token, args: List[Any]): InterResult[Any] = function match {
    case fun: Callable if fun.argumentSize != args.size => unit(Left(ErrorCompiler(token.line, s"Expected ${fun.argumentSize} arguments but got ${args.size}.")))
    case fun: Callable                                  => unit(fun.call(args))
    case _                                              => unit(Left(ErrorCompiler(token.line, "Invalid function type")))
  }
}
