package org.compiler.example
package interpreter.`class`

import error.ErrorCompiler
import interpreter.Environment
import interpreter.function.InterFunction
import lexer.{IDENTIFIER, STRING, Token}
import parser.stmt.Function

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class InterInstanceTest extends AnyFunSuite with Matchers {

  test("methods get existing one, return method") {

    val methodName: Token = Token(IDENTIFIER, "methodName", 1, Some("methodName"))
    val funArg: Token = Token(STRING, "argument", 1, Some("argument"))
    val function: Function = Function(Token(IDENTIFIER, "functionName", 1, Some("functionName")), List(funArg), List())
    val interInstance: InterInstance = new InterInstance(Map(methodName.lexeme -> new InterFunction(function, new Environment())))

    interInstance.get(methodName).map(fun => fun.argumentSize) shouldBe Right(1)
  }

  test("methods get not existing, return error") {

    val interInstance: InterInstance = new InterInstance(Map("methodName" -> new InterFunction(Function(Token(IDENTIFIER, "", 1, None), List(), List()), new Environment())))

    interInstance.get(Token(IDENTIFIER, "methodDoesNotExist", 1, Some("methodDoesNotExist"))) shouldBe Left(ErrorCompiler(1, "Undefined property 'methodDoesNotExist'."))
  }
}
