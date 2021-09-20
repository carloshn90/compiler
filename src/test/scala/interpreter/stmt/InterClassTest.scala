package org.compiler.example
package interpreter.stmt

import error.ErrorCompiler
import helper.TestEnvironmentHelper.defineOrFail
import interpreter.`class`.InterClass
import interpreter.stmt.InterClass.interClass
import interpreter.{Environment, Result}
import lexer.{IDENTIFIER, Token}
import parser.stmt.Function

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterClassTest extends AnyFunSuite with Matchers {

  test("Class interpreter define new class, return environment with the new class") {

    val className: Token = Token(IDENTIFIER, "className", 1, Some("className"))
    val env: Environment = new Environment()
    val resultEnv: Environment = interClass(className, List(), List())(env)._2

    resultEnv.size shouldBe 1
    resultEnv.get("className").map(_.isInstanceOf[InterClass]) shouldBe Right(true)
  }

  test("Class interpreter define new class with a method, return environment with the new class") {

    val className: Token = Token(IDENTIFIER, "className", 1, Some("className"))
    val method: Function = Function(Token(IDENTIFIER, "method", 1, Some("method")), List(), List())
    val env: Environment = new Environment()
    val resultEnv: Environment = interClass(className, List(), List(method))(env)._2

    resultEnv.size shouldBe 1
    resultEnv.get("className").map(_.isInstanceOf[InterClass]) shouldBe Right(true)
    resultEnv.get("className").map(klass => klass.asInstanceOf[InterClass].getMethods.contains("method")) shouldBe Right(true)
  }

  test("Class interpreter define class that already exist, return error") {

    val className: Token = Token(IDENTIFIER, "className", 1, Some("className"))
    val env: Environment = defineOrFail(new Environment(), className, "")
    val (result: Either[ErrorCompiler, Result], resultEnv: Environment) = interClass(className, List(), List())(env)

    resultEnv.size shouldBe 1
    result shouldBe Left(ErrorCompiler(1, "Already a variable with the name: className in this scope."))
  }

}
