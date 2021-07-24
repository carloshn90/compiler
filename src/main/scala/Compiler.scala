package org.compiler.example

import lexer.{Scanner, Token, TokenType}

import error.ErrorCompiler

object Compiler extends App {

  runFile("/Users/carlos/Documents/java/jlox/src/main/resources/main.jlox")

  def runFile(source: String): Unit = {

    val fileSource = scala.io.Source.fromFile(source)
    val fileContent = try fileSource.mkString finally fileSource.close()

    implicit val keywords: Map[String, TokenType] = TokenType.getKeywords
    val scanner: Scanner = new Scanner()
    val tokenList: Either[ErrorCompiler, List[Token]] = scanner.scanTokens(fileContent.toList)

    println(tokenList.map(list => list.mkString("\n")))

  }
}
