package org.compiler.example
package parser

import error.ErrorCompiler
import lexer._
import parser.ParserExpression.ParserExpr

object ParserStatement {

  type ParserResult = List[Token] => Either[ErrorCompiler, List[Stmt]]
  type ParserStmt = List[Token] => (Either[ErrorCompiler, Stmt], List[Token])
  type ConsumerResult = List[Token] => (Either[ErrorCompiler, Token], List[Token])

  def unit(stmt: Either[ErrorCompiler, Stmt]): ParserStmt = tokenList => (stmt, tokenList)

  def parserStmt(parserExpr: ParserExpr)(f:  Expr => Stmt): ParserStmt = tokenList => {

    val (expr, exprTokenList) = parserExpr(tokenList.tail)

    if (exprTokenList.head.tokenType == SEMICOLON) unit(expr.map(e => f(e)))(exprTokenList.tail)
    else unit(Left(ErrorCompiler(tokenList.head.line, s"Expect ';' after expression '${tokenList.head.lexeme}''")))(exprTokenList)
  }

  def parserVarDecl(parserExpr: ParserExpr): ParserStmt = tokenList => {

    val (token, identifierTokenList) = consumer(IDENTIFIER)(_ => "Expect variable name.")(tokenList.tail)

    if (identifierTokenList.head.tokenType == EQUAL) {
      val (expr, exprTokenList) = parserExpr(identifierTokenList.tail)
      (token, expr) match {
        case (Right(t), Right(e)) => initVar(t, e)(exprTokenList)
        case (Left(t), _)         => (Left(t), exprTokenList.tail)
        case (_, Left(e))         => (Left(e), exprTokenList.tail)
      }
    } else {
      token match {
        case Right(t) => initEmptyVar(t)(identifierTokenList)
        case Left(v)  => (Left(v), identifierTokenList.tail)
      }
    }
  }

  private def initVar(token: Token, expr: Expr): ParserStmt = tokenList => {

    val currentToken = tokenList.head

    if (currentToken.tokenType == SEMICOLON) unit(Right(Var(token, expr)))(tokenList.tail)
    else unit(Left(ErrorCompiler(tokenList.head.line, s"Expect ';' after expression '${tokenList.head.lexeme}''")))(tokenList.tail)
  }

  private def initEmptyVar(token: Token): ParserStmt = tokenList => {

    val currentToken = tokenList.head

    if (currentToken.tokenType == SEMICOLON) unit(Right(Var(token, null)))(tokenList.tail)
    else unit(Left(ErrorCompiler(tokenList.head.line, s"Expect ';' after expression '${tokenList.head.lexeme}''")))(tokenList.tail)
  }


  private def consumer(tokenType: TokenType)(f: Token => String): ConsumerResult = tokenList => {

    val currentToken = tokenList.head

    if (tokenType == currentToken.tokenType) (Right(currentToken), tokenList.tail)
    else (Left(ErrorCompiler(currentToken.line, f(currentToken))), tokenList.tail)
  }
}
