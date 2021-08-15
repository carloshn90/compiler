package org.compiler.example
package parser.stmt

import error.ErrorCompiler
import lexer.{EOF, RIGHT_BRACE, Token}
import parser.grammar.GrammarResult
import parser.grammar.ParserGrammar.{ParserExprMonad, ParserGrammar, advance, unit}

object ParserBlock {

  private type ParserBlockResult = List[Token] => (Either[ErrorCompiler, List[Stmt]], List[Token])

  def parserBlock(parserStmt: ParserGrammar[Stmt], stmtList: List[Stmt] = List()): ParserGrammar[Stmt] = tokenList => {

    val currentToken = tokenList.head

    if (currentToken.tokenType != RIGHT_BRACE && currentToken.tokenType != EOF) block(parserStmt, stmtList)(tokenList)
    else createBlock(currentToken, stmtList)(tokenList)
  }

  private def block(parserStmt: ParserGrammar[Stmt], stmtList: List[Stmt]): ParserGrammar[Stmt] = parserStmt
    .flatMap((stmt, _) => stmt match {
      case Right(value) => parserBlock(parserStmt, stmtList :+ value)
      case Left(err)    => unit(Left(err))
    })

  private def createBlock(token: Token, stmtList: List[Stmt]): ParserGrammar[Stmt] =
    if (token.tokenType != RIGHT_BRACE) unit(Left(ErrorCompiler(token.line - 1, "Expect '}' after block.")))
    else advance(unit(GrammarResult.unit(Block(stmtList))))
}
