package org.compiler.example
package parser

import lexer._
import parser.ParserExpression._
import parser.ParserStatement.{ParserResult, ParserStmt, parserStmt, parserVarDecl}
import util.Applicative.eitherApplicative

/**
 * <table border="0">
 *   <tr>
 *     <td>program</td>
 *     <td>→ statement* EOF ;</td>
 *   </tr>
 *   <tr>
 *     <td>declaration</td>
 *     <td>→ varDecl | statement ;</td>
 *   </tr>
 *   <tr>
 *     <td>varDecl</td>
 *     <td>→ "var" IDENTIFIER ( "=" expression )? ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>statement</td>
 *     <td>→ exprStmt | printStmt ;</td>
 *   </tr>
 *   <tr>
 *     <td>exprStmt</td>
 *     <td>→ expression ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>printStmt</td>
 *     <td>→ "print" expression ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>expression</td>
 *     <td>→ equality;</td>
 *   </tr>
 *   <tr>
 *     <td>equality</td>
 *     <td>→ comparison ( ( "!=" | "==" ) comparison )* ;</td>
 *   </tr>
 *   <tr>
 *     <td>comparison</td>
 *     <td>→ term ( ( ">" | ">=" | "<" | "<=" ) term )* ;</td>
 *   </tr>
 *   <tr>
 *     <td>term</td>
 *     <td>→ factor ( ( "-" | "+" ) factor )* ;</td>
 *   </tr>
 *   <tr>
 *     <td>factor</td>
 *     <td>→ unary ( ( "/" | "*" ) unary )* ;</td>
 *   </tr>
 *   <tr>
 *     <td>unary</td>
 *     <td>→ ( "!" | "-" ) unary | primary ;</td>
 *   </tr>
 *   <tr>
 *     <td>primary</td>
 *     <td>→ NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;</td>
 *   </tr>
 * </table>
 */
class Parser {

  def parser(parserStmtList: List[ParserStmt] = List()): ParserResult = tokenList => {

    val currentToken = tokenList.head

    if (currentToken.tokenType != EOF) {
      val (stmt, leftTokenList) = declaration()(tokenList)
      parser(parserStmtList :+ ParserStatement.unit(stmt))(leftTokenList)
    } else
      eitherApplicative.sequence(parserStmtList.map(a => a(tokenList)._1))
  }

  /**
   * declaration → varDecl | statement ;
   */
  def declaration(): ParserStmt = {
    case tokenList@Token(VAR, _, _, _)::_ => varDecl()(tokenList)
    case tokenList@_::_                   => statement()(tokenList)
  }

  /**
   * varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
   */
  def varDecl(): ParserStmt =
    parserVarDecl(expression())

  /**
   * statement → exprStmt | printStmt;
   */
  def statement(): ParserStmt = {
    case tokenList@Token(PRINT, _, _, _)::_ => printStmt()(tokenList)
    case tokenList@_::_                     => exprStmt()(tokenList)
  }

  /**
   * printStmt → "print" expression ";" ;
   */
  def printStmt(): ParserStmt =
    parserStmt(expression())(expr => Print(expr))

  /**
   * exprStmt → expression ";";
   */
  def exprStmt(): ParserStmt =
    parserStmt(expression())(expr => Expression(expr))

  /**
   * expression → equality
   */
  private def expression(): ParserExpr = equality()

  /**
   * equality → comparison ( ( "!=" | "==" ) comparison )* ;
   */
  private def equality(): ParserExpr =
    parserBinary(comparison(), Seq(BANG_EQUAL, EQUAL_EQUAL))(comparison)

  /**
   * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
   */
  private def comparison(): ParserExpr =
    parserBinary(term(), Seq(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL))(term)

  /**
   *term → factor ( ( "-" | "+" ) factor )* ;
   */
  private def term(): ParserExpr =
    parserBinary(factor(), Seq(MINUS, PLUS))(factor)

  /**
   * factor → unary ( ( "/" | "*" ) unary )* ;
   */
  private def factor(): ParserExpr =
    parserBinary(unary(), Seq(SLASH, STAR))(unary)

  /**
   * unary → ( "!" | "-" ) unary | primary ;
   */
  private def unary(): ParserExpr =
    parserUnary(Seq(BANG, MINUS))(unary, primary)

  /**
   * primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
   */
  private def primary(): ParserExpr = {
   case tokenList@Token(FALSE, _, _, _)::_        => parserLiteral(false)(tokenList)
   case tokenList@Token(TRUE, _, _, _)::_         => parserLiteral(true)(tokenList)
   case tokenList@Token(NIL, _, _, _)::_          => parserLiteral(Nil)(tokenList)
   case tokenList@Token(NUMBER, _, _, literal)::_ => parserLiteral(literal.get)(tokenList)
   case tokenList@Token(STRING, _, _, literal)::_ => parserLiteral(literal.get)(tokenList)
   case tokenList@Token(LEFT_PAREN, _, _, _)::_   => parserGrouping()(expression)(tokenList)
   case tokenList@Token(IDENTIFIER, _, _, _)::_   => parserVariable()(tokenList)
  }
}
