package org.compiler.example
package helper

import interpreter.Environment
import lexer.Token

import org.scalatest.Assertions.fail

object TestEnvironmentHelper {

  def defineOrFail(env: Environment, name: Token, value: Any): Environment = {
    env.define(name, value) match {
      case Right(environment) => environment
      case Left(err)          => fail(err.message)
    }
  }
}
