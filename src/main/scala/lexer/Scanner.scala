package org.compiler.example
package lexer

class Scanner(implicit keywords: Map[String, TokenType]) {

  type TokenListResult = Either[String, List[Token]]
  type SourceTokenListResult = Either[String, (List[Char], List[Token])]

  def scanTokens(source: List[Char]): TokenListResult = scanToken(source, List())

  def scanToken(source: List[Char], tokenList: List[Token])(implicit line: Int = 1): TokenListResult = source match {
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

  def addNewToken(tokenList: List[Token], tokenType: TokenType)(implicit line: Int): List[Token] =
    tokenList :+ Token(tokenType, tokenType.lexeme, line)

  def addStringToken(source: List[Char], tokenList: List[Token], value: String = "")
                    (implicit line: Int): SourceTokenListResult = source match {
    case List()     => Left(s"Error in line: $line. Unterminated String.")
    case '\n'::_    => Left(s"Error in line: $line. The String have to be defined in one line.")
    case '"'::tail  => Right(tail, tokenList :+ Token(STRING, value, line))
    case head::tail => addStringToken(tail, tokenList, value + head)
  }

  def addGenericToken(source: List[Char], tokenList: List[Token], value: String = "")
                     (implicit line: Int): SourceTokenListResult = {
    val numeric       = """([0-9]+)""".r
    source match {
      case List()                               => Right(List(), tokenList :+ createTokenFromString(value))
      case '.'::tail if numeric.matches(value)  => addGenericToken(tail, tokenList, value + '.')
      case (' '|'\n'|'.'|';'|')'|'(')::_        => Right(source, tokenList :+ createTokenFromString(value))
      case head::tail                           => addGenericToken(tail, tokenList, value + head)
    }
  }

  def createTokenFromString(value: String)(implicit line: Int): Token = try {
    Token(NUMBER, value.toDouble.toString, line)
  } catch {
    case _: Throwable => Token(keywords.getOrElse(value, IDENTIFIER), value, line)
  }

  def removeComment(source: List[Char]): List[Char] = source match {
    case '\n'::tail => tail
    case _::tail    => removeComment(tail)
  }
}
