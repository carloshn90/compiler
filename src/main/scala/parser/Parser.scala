package org.compiler.example
package parser

import error.ErrorCompiler
import lexer._
import parser.expr.Expr
import parser.expr.ParserAssignment.parserAssignment
import parser.expr.ParserBinary.parserBinary
import parser.expr.ParserCall.parserCall
import parser.expr.ParserGrouping.parserGrouping
import parser.expr.ParserLiteral.parserLiteral
import parser.expr.ParserLogical.parserLogical
import parser.expr.ParserUnary.parserUnary
import parser.expr.ParserVariable.parserVariable
import parser.grammar.GrammarResult.GrammarResult
import parser.grammar.ParserGrammar.{ParserGrammar, unit}
import parser.stmt.ParserBlock.parserBlock
import parser.stmt.ParserExpression.parserExpression
import parser.stmt.ParserFor.parserFor
import parser.stmt.ParserFunction.parserFunction
import parser.stmt.ParserIf.parserIf
import parser.stmt.ParserPrint.parserPrint
import parser.stmt.ParserReturn.parserReturn
import parser.stmt.ParserVar.parserVar
import parser.stmt.ParserWhile.parserWhileStmt
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
 *     <td>→ funDecl | varDecl | statement ;</td>
 *   </tr>
 *   <tr>
 *     <td>funDecl</td>
 *     <td>→ "fun" IDENTIFIER "(" parameters? ")" block ;</td>
 *   </tr>
 *   <tr>
 *     <td>varDecl</td>
 *     <td>→ "var" IDENTIFIER ( "=" expression )? ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>statement</td>
 *     <td>→ forStmt | ifStatement | printStmt | block | returnStmt | whileStmt | exprStmt ;</td>
 *   </tr>
 *   <tr>
 *     <td>returnStmt</td>
 *     <td>→ "return" expression? ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>forStmt</td>
 *     <td>→ "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;</td>
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
 *     <td>whileStmt</td>
 *     <td>→ "while" "(" expression ")" statement ;</td>
 *   </tr>
 *   <tr>
 *     <td>exprStmt</td>
 *     <td>→ expression ";" ;</td>
 *   </tr>
 *   <tr>
 *     <td>expression</td>
 *     <td>→ assignment;</td>
 *   </tr>
 *   <tr>
 *     <td>assignment</td>
 *     <td>→ IDENTIFIER "=" assignment | logicOr ;</td>
 *   </tr>
 *   <tr>
 *     <td>logicOr</td>
 *     <td>→ logicAnd ( "or" logicAnd )*;</td>
 *   </tr>
 *   <tr>
 *     <td>logicAnd</td>
 *     <td>→ equality ( "and" equality )*;</td>
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
 *     <td>→ ( "!" | "-" ) unary | call ;</td>
 *   </tr>
 *   <tr>
 *     <td>call</td>
 *     <td>→ primary ( "(" arguments? ")" )* ;</td>
 *   </tr>
 *   <tr>
 *     <td>arguments</td>
 *     <td>→ expression ( "," expression )* ;</td>
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
      stmt match {
        case Right(value) => parser(parserStmtList :+ unit(Right(value)))(leftTokenList)
        case Left(err)    => Left(err)
      }
    } else
      eitherApplicative.sequence(parserStmtList.map(a => a(tokenList)._1))
  }

  /**
   * declaration → varDecl | statement ;
   */
  def declaration(): ParserGrammar[Stmt] = {
    case Token(FUN, _, _, _)::tail  => funDecl()(tail)
    case Token(VAR, _, _, _)::tail  => varDecl()(tail)
    case tokenList@_::_             => statement()(tokenList)
  }

  /**
   * funDecl → "fun" IDENTIFIER "(" parameters? ")" block ;
   */
  def funDecl(): ParserGrammar[Stmt] =
    parserFunction(blockStmt())

  /**
   * varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
   */
  def varDecl(): ParserGrammar[Stmt] =
    parserVar(expression())

  /**
   * statement → ifStatement | printStmt | returnStmt | block | whileStmt | exprStmt ;
   */
  def statement(): ParserGrammar[Stmt] = {
    case Token(FOR, _, _, _)::tail            => forStmt()(tail)
    case Token(IF, _, _, _)::tail             => ifStatement()(tail)
    case Token(PRINT, _, _, _)::tail          => printStmt()(tail)
    case tokenList@Token(RETURN, _, _, _)::_  => returnStmt()(tokenList)
    case Token(LEFT_BRACE, _, _, _)::tail     => blockStmt()(tail)
    case Token(WHILE, _, _, _)::tail          => whileStmt()(tail)
    case tokenList@_::_                       => exprStmt()(tokenList)
  }

  /**
   * forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
   */
  def forStmt(): ParserGrammar[Stmt] =
    parserFor(varDecl(), exprStmt(), expression(), statement())

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
   * returnStmt → "return" expression? ";" ;
   */
  def returnStmt(): ParserGrammar[Stmt] =
    parserReturn(expression())

  /**
   * block → "{" declaration* "}" ;
   */
  def blockStmt(): ParserGrammar[Stmt] =
    parserBlock(declaration())

  /**
   * whileStmt → "while" "(" expression ")" statement ;
   */
  def whileStmt(): ParserGrammar[Stmt] =
    parserWhileStmt(expression(), statement())

  /**
   * exprStmt → expression ";";
   */
  def exprStmt(): ParserGrammar[Stmt] =
    parserExpression(expression())

  /**
   * expression → assignment;
   */
  def expression(): ParserGrammar[Expr] = assignment()

  /**
   * assignment → IDENTIFIER "=" assignment | logicOr ;
   */
  def assignment(): ParserGrammar[Expr] =
    parserAssignment(logicOr(), Seq(EQUAL))(assignment)

  /**
   * logicOr → logicAnd ( "or" logicAnd )*;
   */
  def logicOr(): ParserGrammar[Expr] =
    parserLogical(logicAnd(), OR)

  /**
   * logicAnd → equality ( "and" equality )*;
   */
  def logicAnd(): ParserGrammar[Expr] =
    parserLogical(equality(), AND)

  /**
   * equality → comparison ( ( "!=" | "==" ) comparison )* ;
   */
  def equality(): ParserGrammar[Expr] =
    parserBinary(comparison(), Seq(BANG_EQUAL, EQUAL_EQUAL))(comparison)

  /**
   * comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
   */
  def comparison(): ParserGrammar[Expr] =
    parserBinary(term(), Seq(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL))(term)

  /**
   *term → factor ( ( "-" | "+" ) factor )* ;
   */
  def term(): ParserGrammar[Expr] =
    parserBinary(factor(), Seq(MINUS, PLUS))(factor)

  /**
   * factor → unary ( ( "/" | "*" ) unary )* ;
   */
  def factor(): ParserGrammar[Expr] =
    parserBinary(unary(), Seq(SLASH, STAR))(unary)

  /**
   * unary → ( "!" | "-" ) unary | call ;
   */
  def unary(): ParserGrammar[Expr] =
    parserUnary(Seq(BANG, MINUS))(unary, call)

  /**
   * <table border="0">
   *   <tr>
   *     <td>call</td>
   *     <td>→ primary ( "(" arguments? ")" )* ;</td>
   *   </tr>
   *   <tr>
   *     <td>arguments</td>
   *     <td>→ expression ( "," expression )* ;</td>
   *   </tr>
   * </table>
   */
  def call(): ParserGrammar[Expr] =
    parserCall(primary(), expression())

  /**
   * primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER ;
   */
  def primary(): ParserGrammar[Expr] = {
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
