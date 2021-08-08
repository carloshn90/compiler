package org.compiler.example
package parser

import lexer._
import parser.expr.ParserAssignment.parserAssignment
import parser.expr.ParserBinary.parserBinary
import parser.expr.ParserExpr.ParserExpr
import parser.expr.ParserGrouping.parserGrouping
import parser.expr.ParserLiteral.parserLiteral
import parser.expr.ParserUnary.parserUnary
import parser.expr.ParserVariable.parserVariable
import parser.stmt.ParserExpression.parserExpression
import parser.stmt.ParserPrint.parserPrint
import parser.stmt.ParserStmt.{ParserResult, ParserStmt, unit}
import parser.stmt.ParserVar.parserVar
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
 *     <td>→ assignment;</td>
 *   </tr>
 *   <tr>
 *     <td>assignment</td>
 *     <td>→ IDENTIFIER "=" assignment | equality ;</td>
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
      parser(parserStmtList :+ unit(stmt))(leftTokenList)
    } else
      eitherApplicative.sequence(parserStmtList.map(a => a(tokenList)._1))
  }

  /**
   * declaration → varDecl | statement ;
   */
  def declaration(): ParserStmt = {
    case Token(VAR, _, _, _)::tail  => varDecl()(tail)
    case tokenList@_::_             => statement()(tokenList)
  }

  /**
   * varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
   */
  def varDecl(): ParserStmt =
    parserVar(expression())

  /**
   * statement → exprStmt | printStmt;
   */
  def statement(): ParserStmt = {
    case Token(PRINT, _, _, _)::tail  => printStmt()(tail)
    case tokenList@_::_               => exprStmt()(tokenList)
  }

  /**
   * printStmt → "print" expression ";" ;
   */
  def printStmt(): ParserStmt =
    parserPrint(expression())

  /**
   * exprStmt → expression ";";
   */
  def exprStmt(): ParserStmt =
    parserExpression(expression())

  /**
   * expression → assignment;
   */
  private def expression(): ParserExpr = assignment()

  /**
   * assignment → IDENTIFIER "=" assignment | equality ;
   */
  private def assignment(): ParserExpr =
    parserAssignment(equality(), Seq(EQUAL))(assignment)

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
   case tokenList@Token(IDENTIFIER, _, _, _)::_   => parserVariable(tokenList.head)(tokenList)
  }
}