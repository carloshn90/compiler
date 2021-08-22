package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import interpreter.Environment
import interpreter.function.InterFunction
import interpreter.stmt.InterFunction.interFunction
import lexer.{IDENTIFIER, Token}
import parser.expr.Variable
import parser.stmt.{Print, Stmt}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterFunctionTest extends AnyFunSuite with Matchers {

  test("InterFunction with parameters should return string") {

    val funName: Token = Token(IDENTIFIER, "sum", 1, Some("sum"))
    val params: List[Token] = List(Token(IDENTIFIER, "a", 1, Some("a")))
    val body: List[Stmt] = List(
      Print(Variable(Token(IDENTIFIER, "a", 1, Some("a")))),
    )

    val (result: Either[ErrorCompiler, List[String]], envResult: Environment) = interFunction(funName, params, body)(new Environment())

    envResult.size shouldBe 1
    envResult.get(funName.lexeme).getOrElse(Nil).isInstanceOf[InterFunction] shouldBe true
    result shouldBe Right(List())
  }

}
