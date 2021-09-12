package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, FUN, LEFT_BRACE, RIGHT_BRACE, Token}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserClass {

  def parserClass(function: ParserGrammar[Stmt]): ParserGrammar[Stmt] = tokenList => {
    val name: Token =  tokenList.head
    if (tokenList.tail.head.tokenType != LEFT_BRACE) (Left(ErrorCompiler(name.line, "Expect '{' before class body.")), tokenList.tail)
    else parserMethods(name, function)(tokenList.tail.tail)
  }

  private def parserMethods(name: Token, function: ParserGrammar[Stmt], methodList: List[Function] = List()): ParserGrammar[Stmt] = {
    case Token(RIGHT_BRACE, _, _, _)::tail      => unit(Right(Class(name, methodList)))(tail)
    case Token(FUN, _, _, _)::tail              => function.flatMap(fun => statementToFunction(fun, name, function, methodList))(tail)
    case tokenList@Token(EOF, _, line, _)::_    => unit(Left(ErrorCompiler(line,"Expect '}' after class body." )))(tokenList)
    case tokenList@Token(_, lexeme, line, _)::_ => unit(Left(ErrorCompiler(line, s"the expression '$lexeme' is not allowed here.")))(tokenList)
  }

  private def statementToFunction(fun: GrammarResult[Stmt], name: Token, function: ParserGrammar[Stmt], methodList: List[Function]): ParserGrammar[Stmt] = fun match {
    case Right(value: Function)    => parserMethods(name, function, methodList :+ value)
    case Left(err)                 => unit(Left(err))
    case _                         => unit(Left(ErrorCompiler(-1, "Error parsing method")))
  }
}
