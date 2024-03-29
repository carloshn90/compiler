package org.compiler.example
package parser.expr

import lexer.{EOF, IDENTIFIER, Token}
import parser.expr.ParserVariable.parserVariable
import parser.grammar.GrammarResult.GrammarResult

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ParserVariableTest extends AnyFunSuite with Matchers {

  test("Parsing a variable, should return a variable with the value") {

    val identifierToken: Token = Token(IDENTIFIER, "variableName", 0, Some("variableName"))
    val tokenList: List[Token] = List(
      identifierToken,
      Token(EOF, "", 1, None)
    )

    val (grammarResult: GrammarResult[Expr], tokenListResult: List[Token]) = parserVariable(identifierToken)(tokenList)

    tokenListResult should have size 1
    grammarResult shouldBe Right(Variable(identifierToken))
  }

}
