package org.compiler.example
package lexer

import error.ErrorCompiler
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers


class ScannerTest extends AnyFunSuite with Matchers {

  case class CorrectCase(source: List[Char], expected: List[Token])
  case class ErrorCase(source: List[Char], expected: ErrorCompiler)

  private val correctCases = Seq(
    CorrectCase(List('('), createTokenList(LEFT_PAREN, "(", 1)),
    CorrectCase(List(')'), createTokenList(RIGHT_PAREN, ")", 1)),
    CorrectCase(List('{'), createTokenList(LEFT_BRACE, "{", 1)),
    CorrectCase(List('}'), createTokenList(RIGHT_BRACE, "}", 1)),
    CorrectCase(List(','), createTokenList(COMMA, ",", 1)),
    CorrectCase(List('.'), createTokenList(DOT, ".", 1)),
    CorrectCase(List('-'), createTokenList(MINUS, "-", 1)),
    CorrectCase(List('+'), createTokenList(PLUS, "+", 1)),
    CorrectCase(List(';'), createTokenList(SEMICOLON, ";", 1)),
    CorrectCase(List('*'), createTokenList(STAR, "*", 1)),
    CorrectCase(List('%'), createTokenList(MODULE, "%", 1)),
    CorrectCase(List('!','='), createTokenList(BANG_EQUAL, "!=", 1)),
    CorrectCase(List('!'), createTokenList(BANG, "!", 1)),
    CorrectCase(List('=','='), createTokenList(EQUAL_EQUAL, "==", 1)),
    CorrectCase(List('='), createTokenList(EQUAL, "=", 1)),
    CorrectCase(List('>','='), createTokenList(GREATER_EQUAL, ">=", 1)),
    CorrectCase(List('>'), createTokenList(GREATER, ">", 1)),
    CorrectCase(List('<','='), createTokenList(LESS_EQUAL, "<=", 1)),
    CorrectCase(List('<'), createTokenList(LESS, "<", 1)),
    CorrectCase(List('/','/','A'), List(Token(EOF, "<eof>", 2))),
    CorrectCase(List('/'), createTokenList(SLASH, "/", 1)),
    CorrectCase(List(' '), List(Token(EOF, "<eof>", 1))),
    CorrectCase(List('\r'), List(Token(EOF, "<eof>", 1))),
    CorrectCase(List('\t'), List(Token(EOF, "<eof>", 1))),
    CorrectCase(List('\n'), List(Token(EOF, "<eof>", 2))),
    CorrectCase("\"string\"".toList, createTokenList(STRING, "string",1, Some("string"))),
    CorrectCase("and".toList, createTokenList(AND, "and",1)),
    CorrectCase("class".toList, createTokenList(CLASS, "class",1)),
    CorrectCase("else".toList, createTokenList(ELSE, "else",1)),
    CorrectCase("false".toList, createTokenList(FALSE, "false",1)),
    CorrectCase("for".toList, createTokenList(FOR, "for",1)),
    CorrectCase("fun".toList, createTokenList(FUN, "fun",1)),
    CorrectCase("if".toList, createTokenList(IF, "if",1)),
    CorrectCase("nil".toList, createTokenList(NIL, "nil",1)),
    CorrectCase("or".toList, createTokenList(OR, "or",1)),
    CorrectCase("print".toList, createTokenList(PRINT, "print",1)),
    CorrectCase("return".toList, createTokenList(RETURN, "return",1)),
    CorrectCase("super".toList, createTokenList(SUPER, "super",1)),
    CorrectCase("this".toList, createTokenList(THIS, "this",1)),
    CorrectCase("true".toList, createTokenList(TRUE, "true",1)),
    CorrectCase("var".toList, createTokenList(VAR, "var",1)),
    CorrectCase("while".toList, createTokenList(WHILE, "while",1)),
    CorrectCase("231".toList, createTokenList(NUMBER, "231", 1, Some(231.0))),
    CorrectCase("3.14169265".toList, createTokenList(NUMBER, "3.14169265", 1, Some(3.14169265))),
    CorrectCase("variable".toList, createTokenList(IDENTIFIER, "variable", 1)),
    CorrectCase("eat.hotDog".toList, List(Token(IDENTIFIER, "eat", 1), Token(DOT, ".", 1), Token(IDENTIFIER,"hotDog", 1), Token(EOF, "<eof>", 1))),
    CorrectCase("eat(".toList, List(Token(IDENTIFIER, "eat", 1), Token(LEFT_PAREN, "(", 1), Token(EOF, "<eof>", 1))),
    CorrectCase("one+two".toList, List(Token(IDENTIFIER, "one", 1), Token(PLUS, "+", 1),Token(IDENTIFIER, "two", 1), Token(EOF, "<eof>", 1)))
  )

  private val errorCases = Seq(
    ErrorCase("\"wrongString".toList, ErrorCompiler(1, "Unterminated String.")),
    ErrorCase("\"wrongString\n\"".toList, ErrorCompiler(1, "The String have to be defined in one line.")),
    ErrorCase("123.Casa".toList, ErrorCompiler(1, "Identifier can't start with a number.")),
  )

  val scanner: Scanner = new Scanner()(TokenType.getKeywords)

  for (CorrectCase(source, expected) <- correctCases) {
    test(s"Check Token $expected by characters $source") {
      val result: Either[ErrorCompiler, List[Token]] = scanner.scanTokens(source)
      result.getOrElse(fail("Either was not Right!")) should equal (expected)
    }
  }

  for (ErrorCase(source, expected) <- errorCases) {
    test(s"Check Error $expected by characters $source") {
      val result: Either[ErrorCompiler, List[Token]] = scanner.scanTokens(source)
      result should equal (Left(expected))
    }
  }

  private def createTokenList(tokenType: TokenType, lexeme: String, line: Int, literal: Option[Any] = None): List[Token] =
    List(Token(tokenType, lexeme, line, literal), Token(EOF, "<eof>", line))
}
