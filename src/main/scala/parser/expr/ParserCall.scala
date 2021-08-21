package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{COMMA, EOF, LEFT_PAREN, RIGHT_PAREN, Token}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserCall {

  def parserCall(primary: ParserGrammar[Expr], expression: ParserGrammar[Expr]): ParserGrammar[Expr] =
    primary.flatMap(expr => parserCallArguments(expression, expr))

  private def parserCallArguments(expression: ParserGrammar[Expr], call: GrammarResult[Expr]): ParserGrammar[Expr] = tokenList => {
    val token: Token = tokenList.head
    if (token.tokenType == LEFT_PAREN) parserArguments(expression, call, List())(tokenList.tail)
    else unit(call)(tokenList)
  }

  private def parserArguments(expression: ParserGrammar[Expr], call: GrammarResult[Expr], arguments: List[Expr]): ParserGrammar[Expr] = {
    case tokenList@Token(_, _, line, _)::_ if arguments.size >= 255         => unit(Left(ErrorCompiler(line, "Can't have more than 255 arguments.")))(tokenList)
    case Token(RIGHT_PAREN, lexeme, line, literal)::tail                    => unit(createCall(call, Token(RIGHT_PAREN, lexeme, line, literal), arguments))(tail)
    case Token(COMMA, _, _, _)::tail if tail.head.tokenType != RIGHT_PAREN  => parserArguments(expression, call, arguments)(tail)
    case tokenList@Token(COMMA, _, line, _)::_                              => unit(Left(ErrorCompiler(line, "Extra ',' is not allowed here.")))(tokenList)
    case tokenList@Token(EOF, _, line, _)::_                                => unit(Left(ErrorCompiler(line, "Expect ')' after arguments.")))(tokenList)
    case tokenList@_::_                                                     => expression.flatMap(arg => parserArgument(arg, arguments)(args => parserArguments(expression, call, args)))(tokenList)
  }

  private def parserArgument(argument: GrammarResult[Expr], arguments: List[Expr])(f: List[Expr] => ParserGrammar[Expr]): ParserGrammar[Expr] = argument match {
    case Right(a) => f(arguments :+ a)
    case Left(err) => unit(Left(err))
  }

  private def createCall(calle: GrammarResult[Expr], parent: Token, arguments: List[Expr]): GrammarResult[Expr] =
    calle.map(c => Call(c, parent, arguments))
}
