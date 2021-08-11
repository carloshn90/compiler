package org.compiler.example

import error.ErrorCompiler
import interpreter.{Environment, Interpreter}
import lexer.{Scanner, Token, TokenType}
import parser.Parser
import parser.stmt.Stmt

object Compiler extends App {

  runFile("/Users/carlos/Documents/Scala/compilator/src/main/resources/main.cehn")

  def runFile(source: String): Unit = {

    val fileSource = scala.io.Source.fromFile(source)
    val fileContent = try fileSource.mkString finally fileSource.close()

    implicit val keywords: Map[String, TokenType] = TokenType.getKeywords
    val scanner: Scanner = new Scanner()
    val parser: Parser = new Parser()
    val interpreter: Interpreter = new Interpreter()
    val tokenList: Either[ErrorCompiler, List[Token]] = scanner.scanTokens(fileContent.toList)

    val expr: Either[ErrorCompiler, List[String]] = tokenList
      .flatMap(parser.parser())
      .flatMap((stmtList: List[Stmt]) => interpreter.interpreter(stmtList, Right(List()))(new Environment)._1)

    expr.map(_.reverse)
      .map(list => list.map(println))
      .getOrElse(println)
  }
}
