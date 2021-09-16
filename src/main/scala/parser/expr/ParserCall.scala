package org.compiler.example
package parser.expr

import error.ErrorCompiler
import lexer.{COMMA, DOT, EOF, IDENTIFIER, LEFT_PAREN, RIGHT_PAREN, Token}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

object ParserCall {

  def parserCall(primary: ParserGrammar[Expr], expression: ParserGrammar[Expr]): ParserGrammar[Expr] = {
    case tokenList@Token(IDENTIFIER, lexeme, line, literal)::_ => primary.flatMap(expr => parserCallArguments(expression, expr, Token(IDENTIFIER, lexeme, line, literal)))(tokenList)
    case tokenList@_                                           => primary(tokenList)
  }

  private def parserCallArguments(expression: ParserGrammar[Expr], call: GrammarResult[Expr], parent: Token): ParserGrammar[Expr] = {
    case Token(LEFT_PAREN, _, _, _)::tail       => parserArguments(expression, call, parent, List())(tail)
    case Token(DOT, _, _, _)::identifier::tail  => parserCallArguments(expression, createGet(call, identifier), identifier)(tail)
    case tokenList@_                            => unit(call)(tokenList)
  }

  private def parserArguments(expression: ParserGrammar[Expr], call: GrammarResult[Expr], parent: Token, arguments: List[Expr]): ParserGrammar[Expr] = {
    case tokenList@Token(_, _, line, _)::_ if arguments.size >= 255         => unit(Left(ErrorCompiler(line, "Can't have more than 255 arguments.")))(tokenList)
    case Token(RIGHT_PAREN, _, _, _)::tail                                  => parserCallArguments(expression, createCall(call, parent, arguments), parent)(tail)
    case Token(COMMA, _, _, _)::tail if tail.head.tokenType != RIGHT_PAREN  => parserArguments(expression, call, parent, arguments)(tail)
    case tokenList@Token(COMMA, _, line, _)::_                              => unit(Left(ErrorCompiler(line, "Extra ',' is not allowed here.")))(tokenList)
    case tokenList@Token(EOF, _, line, _)::_                                => unit(Left(ErrorCompiler(line, "Expect ')' after arguments.")))(tokenList)
    case tokenList@_::_                                                     => expression.flatMap(arg => parserArgument(arg, arguments)(args => parserArguments(expression, call, parent, args)))(tokenList)
  }

  private def parserArgument(argument: GrammarResult[Expr], arguments: List[Expr])(f: List[Expr] => ParserGrammar[Expr]): ParserGrammar[Expr] = argument match {
    case Right(a) => f(arguments :+ a)
    case Left(err) => unit(Left(err))
  }

  private def createCall(calle: GrammarResult[Expr], parent: Token, arguments: List[Expr]): GrammarResult[Expr] =
    calle.map(fun => Call(fun, parent, arguments))

  private def createGet(calle: GrammarResult[Expr], parent: Token): GrammarResult[Expr] = parent match {
    case Token(IDENTIFIER, _, _, _) => calle.map(expr => Get(expr, parent))
    case Token(_, _, line, _)       => Left(ErrorCompiler(line, "Expect property name after '.'."))
  }
}
