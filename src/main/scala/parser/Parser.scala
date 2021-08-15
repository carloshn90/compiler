package org.compiler.example
package parser

import error.ErrorCompiler
import lexer._
import parser.expr.Expr
import parser.expr.ParserAssignment.parserAssignment
import parser.expr.ParserBinary.parserBinary
import parser.expr.ParserGrouping.parserGrouping
import parser.expr.ParserLiteral.parserLiteral
import parser.expr.ParserUnary.parserUnary
import parser.expr.ParserVariable.parserVariable
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserGrammar, unit}
import parser.stmt.ParserBlock.parserBlock
import parser.stmt.ParserExpression.parserExpression
import parser.stmt.ParserIf.parserIf
import parser.stmt.ParserPrint.parserPrint
import parser.stmt.ParserVar.parserVar
import parser.stmt.Stmt
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
 *     <td>→ ifStatement | printStmt | block | exprStmt;</td>
 *   </tr>
 *   <tr>
 *     <td>exprStmt</td>
 *     <td>→ expression ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>ifStatement</td>
 *     <td>→ "if" "(" expression ")" statement ("else" statement)? ;</td>
 *   </tr>
 *   <tr>
 *     <td>printStmt</td>
 *     <td>→ "print" expression ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>block</td>
 *     <td>→ "{" declaration* "}" ;</td>
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

  type ParserResult = List[Token] => Either[ErrorCompiler, List[Stmt]]

  def parser(parserStmtList: List[ParserGrammar[Stmt]] = List()): ParserResult = tokenList => {

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
  def declaration(): ParserGrammar[Stmt] = {
    case Token(VAR, _, _, _)::tail  => varDecl()(tail)
    case tokenList@_::_             => statement()(tokenList)
  }

  /**
   * varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
   */
  def varDecl(): ParserGrammar[Stmt] =
    parserVar(expression())

  /**
   * statement → ifStatement | printStmt | block | exprStmt ;
   */
  def statement(): ParserGrammar[Stmt] = {
    case Token(IF, _, _, _)::tail         => ifStatement()(tail)
    case Token(PRINT, _, _, _)::tail      => printStmt()(tail)
    case Token(LEFT_BRACE, _, _, _)::tail => blockStmt()(tail)
    case tokenList@_::_                   => exprStmt()(tokenList)
  }

  /**
   * ifStatement → "if" "(" expression ")" statement ("else" statement)? ;
   */
  def ifStatement(): ParserGrammar[Stmt] =
    parserIf(expression(), statement())

  /**
   * printStmt → "print" expression ";" ;
   */
  def printStmt(): ParserGrammar[Stmt] =
    parserPrint(expression())

  /**
   * exprStmt → expression ";";
   */
  def exprStmt(): ParserGrammar[Stmt] =
    parserExpression(expression())

  /**
   * block → "{" declaration* "}" ;
   */
  def blockStmt(): ParserGrammar[Stmt] =
    parserBlock(declaration())

  /**
   * expression → assignment;
   */
  def expression(): ParserGrammar[Expr] = assignment()

  /**
   * assignment → IDENTIFIER "=" assignment | equality ;
   */
  private def assignment(): ParserGrammar[Expr] =
    parserAssignment(equality(), Seq(EQUAL))(assignment)

  /**
   * equality → comparison ( ( "!=" | "==" ) comparison )* ;
   */
  private def equality(): ParserGrammar[Expr] =
    parserBinary(comparison(), Seq(BANG_EQUAL, EQUAL_EQUAL))(comparison)

  /**
   * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
   */
  private def comparison(): ParserGrammar[Expr] =
    parserBinary(term(), Seq(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL))(term)

  /**
   *term → factor ( ( "-" | "+" ) factor )* ;
   */
  private def term(): ParserGrammar[Expr] =
    parserBinary(factor(), Seq(MINUS, PLUS))(factor)

  /**
   * factor → unary ( ( "/" | "*" ) unary )* ;
   */
  private def factor(): ParserGrammar[Expr] =
    parserBinary(unary(), Seq(SLASH, STAR))(unary)

  /**
   * unary → ( "!" | "-" ) unary | primary ;
   */
  private def unary(): ParserGrammar[Expr] =
    parserUnary(Seq(BANG, MINUS))(unary, primary)

  /**
   * primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
   */
  private def primary(): ParserGrammar[Expr] = {
   case tokenList@Token(FALSE, _, _, _)::_        => parserLiteral(false)(tokenList)
   case tokenList@Token(TRUE, _, _, _)::_         => parserLiteral(true)(tokenList)
   case tokenList@Token(NIL, _, _, _)::_          => parserLiteral(Nil)(tokenList)
   case tokenList@Token(NUMBER, _, _, literal)::_ => parserLiteral(literal.get)(tokenList)
   case tokenList@Token(STRING, _, _, literal)::_ => parserLiteral(literal.get)(tokenList)
   case tokenList@Token(LEFT_PAREN, _, _, _)::_   => parserGrouping()(expression)(tokenList)
   case tokenList@Token(IDENTIFIER, _, _, _)::_   => parserVariable(tokenList.head)(tokenList)
   case tokenList@_                               => unit(errorParsingExpr(tokenList.head))(tokenList)
  }

  private def errorParsingExpr(token: Token): GrammarResult[Expr] =
    Left(ErrorCompiler(token.line, s"the expression '${token.lexeme}' is not allowed here."))
}
