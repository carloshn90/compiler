package org.compiler.example
package parser

import lexer._
import parser.ParserExpression._

/**
 * <table border="0">
 *   <tr>
 *     <td>expression</td>
 *     <td>→ equality</td>
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
 *     <td>→ NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;</td>
 *   </tr>
 * </table>
 */
class Parser {

  def parser(): ParserExpr = expression()

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
   * primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" ;
   */
  private def primary(): ParserExpr = {
   case tokenList@Token(FALSE, _, _, _)::_        => parserLiteral(false)(tokenList)
   case tokenList@Token(TRUE, _, _, _)::_         => parserLiteral(true)(tokenList)
   case tokenList@Token(NIL, _, _, _)::_          => parserLiteral(Nil)(tokenList)
   case tokenList@Token(NUMBER, _, _, literal)::_ => parserLiteral(literal.get)(tokenList)
   case tokenList@Token(STRING, _, _, literal)::_ => parserLiteral(literal.get)(tokenList)
   case tokenList@Token(LEFT_PAREN, _, _, _)::_   => parserGrouping()(expression)(tokenList)
  }
}
