package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{COMMA, IDENTIFIER, LEFT_BRACE, LEFT_PAREN, RIGHT_PAREN, Token}
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, unit}

import scala.annotation.tailrec

object ParserFunction {

  def parserFunction(block: ParserGrammar[Stmt]): ParserGrammar[Stmt] = tokenList => {
    getFunctionName(tokenList.head) match {
      case Right(funName) => parserFunctionWithName(block, funName)(tokenList.tail)
      case Left(err)      => unit(Left(err))(tokenList)
    }
  }

  private def parserFunctionWithName(block: ParserGrammar[Stmt], functionName: Token): ParserGrammar[Stmt] = tokenList => {
    checkLeftParenthesis().flatMap { _ => tokenList =>
      val (parameters, paramTokens) = getParameters(tokenList, List())
      parameters match {
        case Right(params) => parserBody(block).map(b => createFunction(functionName, params, b))(paramTokens)
        case Left(err) => unit(Left(err))(paramTokens)
      }
    }(tokenList)
  }

  private def getFunctionName(token: Token): Either[ErrorCompiler, Token] =
    if (token.tokenType == IDENTIFIER) Right(token)
    else Left(ErrorCompiler(token.line, "Expect function name."))

  @tailrec
  private def getParameters(tokenList: List[Token], parameters: List[Token]): (Either[ErrorCompiler, List[Token]], List[Token]) = tokenList match {
    case Token(COMMA, _, line, _)::_ if parameters.size >= 255  => (Left(ErrorCompiler(line, "Can't have more than 255 parameters")), tokenList)
    case Token(COMMA, _, _, _)::tail                            => getParameters(tail, parameters)
    case Token(IDENTIFIER, lexeme, line, literal)::tail         => getParameters(tail, parameters :+ Token(IDENTIFIER, lexeme, line, literal))
    case Token(RIGHT_PAREN, _, _, _)::tail                      => (Right(parameters), tail)
    case Token(_, _, line, _)::tail                             => (Left(ErrorCompiler(line, "Expect ')' after parameters.")), tail)
  }

  private def parserBody(block: ParserGrammar[Stmt]): ParserGrammar[Block] = tokenList => {
    val token: Token = tokenList.head
    if (token.tokenType == LEFT_BRACE) parserBlock(block)(tokenList.tail)
    else unit(Left(ErrorCompiler(token.line, "Expect '{' before function body.")))(tokenList)
  }

  private def parserBlock(block: ParserGrammar[Stmt]): ParserGrammar[Block] = block.map {
    case Right(b: Block) => Right(b)
    case Left(err)       => Left(err)
  }

  private def createFunction(funName: Token, params: List[Token], body: GrammarResult[Block]): GrammarResult[Stmt] =
    body.map(block => Function(funName, params, block.statements))

  private def checkLeftParenthesis(): ParserGrammar[Stmt] = tokenList => {
    val token: Token = tokenList.head
    if (token.tokenType != LEFT_PAREN) unit(Left(ErrorCompiler(token.line, "Expect '(' after function name.")))(tokenList)
    else unit(Right(Stmt.None))(tokenList.tail)
  }
}
