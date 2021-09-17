package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer._
import parser.expr.Expr
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}
import parser.stmt.ParserVar.parserVar

object ParserClass {

  def parserClass(expression: ParserGrammar[Expr], function: ParserGrammar[Stmt]): ParserGrammar[Stmt] = {
    case name::Token(LEFT_BRACE, _, _, _)::tail => parserMethods(expression, name, function)(tail)
    case Token(_, _, line, _)::tail             => (Left(ErrorCompiler(line, "Expect '{' before class body.")), tail)
  }

  private def parserMethods(expression: ParserGrammar[Expr], name: Token, function: ParserGrammar[Stmt], varList: List[Var] = List(), methodList: List[Function] = List()): ParserGrammar[Stmt] = {
    case Token(RIGHT_BRACE, _, _, _)::tail      => unit(Right(Class(name, varList, methodList)))(tail)
    case Token(FUN, _, _, _)::tail              => function.flatMap(fun => statementToFunction(expression, fun, name, function, varList, methodList))(tail)
    case Token(VAR, _, _, _)::tail              => parserVar(expression).flatMap(va => statementToVar(expression, va, name, function, varList, methodList))(tail)
    case tokenList@Token(EOF, _, line, _)::_    => unit(Left(ErrorCompiler(line,"Expect '}' after class body." )))(tokenList)
    case tokenList@Token(_, lexeme, line, _)::_ => unit(Left(ErrorCompiler(line, s"the expression '$lexeme' is not allowed here.")))(tokenList)
  }

  private def statementToVar(expression: ParserGrammar[Expr], variable: GrammarResult[Stmt], name: Token, function: ParserGrammar[Stmt], varList: List[Var], methodList: List[Function]): ParserGrammar[Stmt] = variable match {
    case Right(value: Var)  => parserMethods(expression, name, function, varList :+ value, methodList)
    case Left(err)          => unit(Left(err))
    case _                  => unit(Left(ErrorCompiler(-1, "Error parsing var")))
  }

  private def statementToFunction(expression: ParserGrammar[Expr], fun: GrammarResult[Stmt], name: Token, function: ParserGrammar[Stmt], varList: List[Var], methodList: List[Function]): ParserGrammar[Stmt] = fun match {
    case Right(value: Function)    => parserMethods(expression, name, function, varList, methodList :+ value)
    case Left(err)                 => unit(Left(err))
    case _                         => unit(Left(ErrorCompiler(-1, "Error parsing method")))
  }
}
