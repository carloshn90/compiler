package org.compiler.example
package lexer

object TokenType {

  def getKeywords: Map[String, TokenType] = Map(
    "and"     -> AND,
    "class"   -> CLASS,
    "else"    -> ELSE,
    "false"   -> FALSE,
    "for"     -> FOR,
    "fun"     -> FUN,
    "if"      -> IF,
    "nil"     -> NIL,
    "or"      -> OR,
    "print"   -> PRINT,
    "return"  -> RETURN,
    "super"   -> SUPER,
    "this"    -> THIS,
    "true"    -> TRUE,
    "var"     -> VAR,
    "while"   -> WHILE,
  )
}

sealed trait TokenType {
  def name: String
  def lexeme: String
}

// Single-character tokens.
case object LEFT_PAREN extends TokenType {
  override def name: String = "LEFT_PAREN"
  override def lexeme: String = "("
}

case object RIGHT_PAREN extends TokenType {
  override def name: String = "RIGHT_PAREN"
  override def lexeme: String = ")"
}

case object LEFT_BRACE extends TokenType {
  override def name: String = "LEFT_BRACE"
  override def lexeme: String = "{"
}

case object RIGHT_BRACE extends TokenType {
  override def name: String = "RIGHT_BRACE"
  override def lexeme: String = "}"
}

case object COMMA extends TokenType {
  override def name: String = "COMMA"
  override def lexeme: String = ","
}

case object DOT extends TokenType {
  override def name: String = "DOT"
  override def lexeme: String = "."
}

case object MINUS extends TokenType {
  override def name: String = "MINUS"
  override def lexeme: String = "-"
}

case object PLUS extends TokenType {
  override def name: String = "PLUS"
  override def lexeme: String = "+"
}

case object SEMICOLON extends TokenType {
  override def name: String = "SEMICOLON"
  override def lexeme: String = ";"
}

case object SLASH extends TokenType {
  override def name: String = "SLASH"
  override def lexeme: String = "/"
}

case object STAR extends TokenType {
  override def name: String = "STAR"
  override def lexeme: String = "*"
}

// One or two character tokens.
case object BANG extends TokenType {
  override def name: String = "BANG"
  override def lexeme: String = "!"
}

case object BANG_EQUAL extends TokenType {
  override def name: String = "BANG_EQUAL"
  override def lexeme: String = "!="
}

case object EQUAL extends TokenType {
  override def name: String = "EQUAL"
  override def lexeme: String = "="
}

case object EQUAL_EQUAL extends TokenType {
  override def name: String = "EQUAL_EQUAL"
  override def lexeme: String = "=="
}

case object GREATER extends TokenType {
  override def name: String = "GREATER"
  override def lexeme: String = ">"
}

case object GREATER_EQUAL extends TokenType {
  override def name: String = "GREATER_EQUAL"
  override def lexeme: String = ">="
}

case object LESS extends TokenType {
  override def name: String = "LESS"
  override def lexeme: String = "<"
}

case object LESS_EQUAL extends TokenType {
  override def name: String = "LESS_EQUAL"
  override def lexeme: String = "<="
}

// Literals.
case object IDENTIFIER extends TokenType {
  override def name: String = "IDENTIFIER"
  override def lexeme: String = ""
}

case object STRING extends TokenType {
  override def name: String = "STRING"
  override def lexeme: String = ""
}

case object NUMBER extends TokenType {
  override def name: String = "NUMBER"
  override def lexeme: String = ""
}

// Keywords.
case object AND extends TokenType {
  override def name: String = "AND"
  override def lexeme: String = "and"
}

case object CLASS extends TokenType {
  override def name: String = "CLASS"
  override def lexeme: String = "class"
}

case object ELSE extends TokenType {
  override def name: String = "ELSE"
  override def lexeme: String = "else"
}

case object FALSE extends TokenType {
  override def name: String = "FALSE"
  override def lexeme: String = "false"
}

case object FUN extends TokenType {
  override def name: String = "FUN"
  override def lexeme: String = "fun"
}

case object FOR extends TokenType {
  override def name: String = "FOR"
  override def lexeme: String = "for"
}

case object IF extends TokenType {
  override def name: String = "IF"
  override def lexeme: String = "if"
}

case object NIL extends TokenType {
  override def name: String = "NIL"
  override def lexeme: String = "nil"
}

case object OR extends TokenType {
  override def name: String = "OR"
  override def lexeme: String = "or"
}

case object PRINT extends TokenType {
  override def name: String = "PRINT"
  override def lexeme: String = "print"
}

case object RETURN extends TokenType {
  override def name: String = "RETURN"
  override def lexeme: String = "return"
}

case object SUPER extends TokenType {
  override def name: String = "SUPER"
  override def lexeme: String = "super"
}

case object THIS extends TokenType {
  override def name: String = "THIS"
  override def lexeme: String = "this"
}

case object TRUE extends TokenType {
  override def name: String = "TRUE"
  override def lexeme: String = "true"
}

case object VAR extends TokenType {
  override def name: String = "VAR"
  override def lexeme: String = "var"
}

case object WHILE extends TokenType {
  override def name: String = "WHILE"
  override def lexeme: String = "while"
}

case object EOF extends TokenType {
  override def name: String = "EOF"
  override def lexeme: String = "<eof>"
}
