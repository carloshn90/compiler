package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.stmt.InterWhile.interWhile
import lexer.{IDENTIFIER, LESS, PLUS, Token}
import parser.expr.{Assign, Binary, Expr, Literal, Variable}
import parser.stmt.{Block, Expression, Print, Stmt}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterWhileTest extends AnyFunSuite with Matchers {

  test("Interpreting while with condition a < 2 increase a by 1 and print result, should return two values 1 and 0") {

    val aVar: Token = Token(IDENTIFIER, "a", 1, Some("a"))
    val condition: Expr = Binary(Variable(aVar), Token(LESS, "<", 1, None), Literal(2))
    val body: Stmt =  Block(List(
      Print(Variable(aVar)),
      Expression(Assign(aVar, Binary(Variable(Token(IDENTIFIER, "a", 3, Some("a"))), Token(PLUS, "+", 3, None), Literal(1.0))))
    ))
    val env: Environment = new Environment().define("a", 0.0)

    val (intResult: Either[ErrorCompiler, List[String]], envResult: Environment) = interWhile(condition, body)(env)

    envResult.size shouldBe 1
    intResult shouldBe Right(List("1", "0"))
  }

  test("Error interpreting condition, should return error") {

    val aVar: Token = Token(IDENTIFIER, "a", 1, Some("a"))
    val condition: Expr = Binary(Variable(aVar), Token(LESS, "<", 1, None), Literal(1))
    val body: Stmt =  Block(List(
    ))
    val env: Environment = new Environment()

    val intResult: Either[ErrorCompiler, List[String]] = interWhile(condition, body)(env)._1

    intResult shouldBe Left(ErrorCompiler(1, "Undefined variable 'a'."))
  }

  test("Error interpreting body, should return error") {

    val aVar: Token = Token(IDENTIFIER, "a", 1, Some("a"))
    val bVar: Token = Token(IDENTIFIER, "b", 1, Some("b"))
    val condition: Expr = Binary(Variable(aVar), Token(LESS, "<", 1, None), Literal(1))
    val body: Stmt =  Block(List(
      Print(Variable(bVar))
    ))
    val env: Environment = new Environment().define("a", 0.0)

    val intResult: Either[ErrorCompiler, List[String]] = interWhile(condition, body)(env)._1

    intResult shouldBe Left(ErrorCompiler(1, "Undefined variable 'b'."))
  }

  test("No interpreting body if condition is false, should return empty list") {


    val bVar: Token = Token(IDENTIFIER, "b", 1, Some("b"))
    val condition: Expr = Literal(false)
    val body: Stmt =  Block(List(
      Print(Variable(bVar))
    ))
    val env: Environment = new Environment()

    val intResult: Either[ErrorCompiler, List[String]] = interWhile(condition, body)(env)._1

    intResult shouldBe Right(List())
  }

}
