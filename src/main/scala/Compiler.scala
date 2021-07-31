package org.compiler.example

import error.ErrorCompiler
import interpreter.Interpreter
import lexer.{Scanner, Token, TokenType}
import parser.Parser

object Compiler extends App {

  runFile("/Users/carlos/Documents/java/jlox/src/main/resources/main.jlox")

  def runFile(source: String): Unit = {

    val fileSource = scala.io.Source.fromFile(source)
    val fileContent = try fileSource.mkString finally fileSource.close()

    implicit val keywords: Map[String, TokenType] = TokenType.getKeywords
    val scanner: Scanner = new Scanner()
    val parser: Parser = new Parser()
    val interpreter: Interpreter = new Interpreter()
    val tokenList: Either[ErrorCompiler, List[Token]] = scanner.scanTokens(fileContent.toList)

    val expr: Either[ErrorCompiler, String] = tokenList
      .map(parser.parser())
      .flatMap(_._1)
      .flatMap(interpreter.interpreter)

    println(expr)
  }
}
