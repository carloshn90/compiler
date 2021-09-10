package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, RETURN, RIGHT_BRACE, Token}
import parser.expr.Expr
import parser.grammar.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, advance, unit}
import parser.stmt.ParserReturn.parserReturn

object ParserBlock {

  private type ParserBlockResult = List[Token] => (Either[ErrorCompiler, List[Stmt]], List[Token])

  def parserBlock(expression: ParserGrammar[Expr], declaration: ParserGrammar[Stmt], stmtList: List[Stmt] = List()): ParserGrammar[Stmt] = tokenList => {

    val currentToken = tokenList.head

    if (currentToken.tokenType != RIGHT_BRACE && currentToken.tokenType != EOF) block(currentToken, expression, declaration, stmtList)(tokenList)
    else createBlock(currentToken, stmtList)(tokenList)
  }

  private def block(token: Token, expression: ParserGrammar[Expr], declaration: ParserGrammar[Stmt], stmtList: List[Stmt]): ParserGrammar[Stmt] = token match {
    case Token(RETURN, _, _, _) => parserBlockReturn(expression, declaration, stmtList)
    case _                      => parserDeclaration(expression, declaration, stmtList)
  }

  private def parserBlockReturn(expression: ParserGrammar[Expr], declaration: ParserGrammar[Stmt], stmtList: List[Stmt]): ParserGrammar[Stmt] = parserReturn(expression)
    .flatMap {
      case Right(value) => parserBlock(expression, declaration, stmtList :+ value)
      case Left(err)    => unit(Left(err))
    }

  private def parserDeclaration(expression: ParserGrammar[Expr], declaration: ParserGrammar[Stmt], stmtList: List[Stmt]): ParserGrammar[Stmt] = declaration
    .flatMap {
      case Right(value) => parserBlock(expression, declaration, stmtList :+ value)
      case Left(err)    => unit(Left(err))
    }

  private def createBlock(token: Token, stmtList: List[Stmt]): ParserGrammar[Stmt] =
    if (token.tokenType != RIGHT_BRACE) unit(Left(ErrorCompiler(token.line - 1, "Expect '}' after block.")))
    else advance(unit(GrammarResult.unit(Block(stmtList))))
}
