package org.compiler.example
package lexer


import error.ErrorCompiler

import scala.annotation.tailrec

class Scanner(implicit keywords: Map[String, TokenType]) {

  type TokenListResult = Either[ErrorCompiler, List[Token]]
  type SourceTokenListResult = Either[ErrorCompiler, (List[Char], List[Token])]

  def scanTokens(source: List[Char]): TokenListResult = scanToken(source, List())

  private def scanToken(source: List[Char], tokenList: List[Token])(implicit line: Int = 1): TokenListResult = source match {
    case List()                 => Right(addNewToken(tokenList, EOF))
    case '\n'::tail             => scanToken(tail, tokenList)(line + 1)
    case '('::tail              => scanToken(tail, addNewToken(tokenList, LEFT_PAREN))
    case ')'::tail              => scanToken(tail, addNewToken(tokenList, RIGHT_PAREN))
    case '{'::tail              => scanToken(tail, addNewToken(tokenList, LEFT_BRACE))
    case '}'::tail              => scanToken(tail, addNewToken(tokenList, RIGHT_BRACE))
    case ','::tail              => scanToken(tail, addNewToken(tokenList, COMMA))
    case '.'::tail              => scanToken(tail, addNewToken(tokenList, DOT))
    case '-'::tail              => scanToken(tail, addNewToken(tokenList, MINUS))
    case '+'::tail              => scanToken(tail, addNewToken(tokenList, PLUS))
    case ';'::tail              => scanToken(tail, addNewToken(tokenList, SEMICOLON))
    case '*'::tail              => scanToken(tail, addNewToken(tokenList, STAR))
    case '%'::tail              => scanToken(tail, addNewToken(tokenList, MODULE))
    case '&'::'&'::tail         => scanToken(tail, addNewToken(tokenList, AND))
    case '|'::'|'::tail         => scanToken(tail, addNewToken(tokenList, OR))
    case '!'::'='::tail         => scanToken(tail, addNewToken(tokenList, BANG_EQUAL))
    case '!'::tail              => scanToken(tail, addNewToken(tokenList, BANG))
    case '='::'='::tail         => scanToken(tail, addNewToken(tokenList, EQUAL_EQUAL))
    case '='::tail              => scanToken(tail, addNewToken(tokenList, EQUAL))
    case '<'::'='::tail         => scanToken(tail, addNewToken(tokenList, LESS_EQUAL))
    case '<'::tail              => scanToken(tail, addNewToken(tokenList, LESS))
    case '>'::'='::tail         => scanToken(tail, addNewToken(tokenList, GREATER_EQUAL))
    case '>'::tail              => scanToken(tail, addNewToken(tokenList, GREATER))
    case '/'::'/'::tail         => scanToken(removeComment(tail), tokenList)(line + 1)
    case '/'::tail              => scanToken(tail, addNewToken(tokenList, SLASH))
    case (' '|'\r'|'\t')::tail  => scanToken(tail, tokenList)
    case '"'::tail              => addStringToken(tail, tokenList).flatMap((scanToken _).tupled)
    case _                      => addGenericToken(source, tokenList).flatMap((scanToken _).tupled)
  }

  private def addNewToken(tokenList: List[Token], tokenType: TokenType)(implicit line: Int): List[Token] =
    tokenList :+ Token(tokenType, tokenType.lexeme, line)

  @tailrec
  private def addStringToken(source: List[Char], tokenList: List[Token], value: String = "")
                            (implicit line: Int): SourceTokenListResult = source match {
    case List()     => Left(ErrorCompiler(line, "Unterminated String."))
    case '\n'::_    => Left(ErrorCompiler(line, "The String have to be defined in one line."))
    case '"'::tail  => Right(tail, tokenList :+ Token(STRING, value, line, Some(value)))
    case head::tail => addStringToken(tail, tokenList, value + head)
  }

  @tailrec
  private def addGenericToken(source: List[Char], tokenList: List[Token], value: String = "")
                             (implicit line: Int): SourceTokenListResult = {
    val numeric         = """([0-9]+)""".r
    val isReservedChar  = """([\n(){},.\-+;*!=<>/ \r\t"])""".r
    source match {
      case List()                               => createNumberOrStringToken(value).map(token => (List(), tokenList :+ token))
      case '.'::tail if numeric.matches(value)  => addGenericToken(tail, tokenList, value + '.')
      case isReservedChar(_)::_                 => createNumberOrStringToken(value).map(token => (source, tokenList :+ token))
      case head::tail                           => addGenericToken(tail, tokenList, value + head)
    }
  }

  private def createNumberOrStringToken(value: String)(implicit line: Int): Either[ErrorCompiler, Token] = try {
    Right(Token(NUMBER, value, line, Some(value.toDouble)))
  } catch {
    case _: Throwable => createIdentifierToken(value)
  }

  private def createIdentifierToken(identifier: String)(implicit line: Int): Either[ErrorCompiler, Token] =
    if (identifier.head.isDigit) Left(ErrorCompiler(line, "Identifier can't start with a number."))
    else Right(Token(keywords.getOrElse(identifier, IDENTIFIER), identifier, line))

  @tailrec
  private def removeComment(source: List[Char]): List[Char] = source match {
    case List()     => source
    case '\n'::tail => tail
    case _::tail    => removeComment(tail)
  }
}
