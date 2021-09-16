package org.compiler.example
package interpreter.`class`

import error.ErrorCompiler
import lexer.{IDENTIFIER, Token}

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterInstanceTest extends AnyFunSuite with Matchers {

  test("methods get existing one, return method") {

    val interInstance: InterInstance = new InterInstance(Map("methodName" -> 2))

    interInstance.get(Token(IDENTIFIER, "methodName", 1, Some("methodName"))) shouldBe Right(2)
  }

  test("methods get not existing, return error") {

    val interInstance: InterInstance = new InterInstance(Map("methodName" -> 2))

    interInstance.get(Token(IDENTIFIER, "methodDoesNotExist", 1, Some("methodDoesNotExist"))) shouldBe Left(ErrorCompiler(1, "Undefined property 'methodDoesNotExist'."))
  }
}
