package org.compiler.example
package interpreter.function

import interpreter.InterResult.InterResult
import interpreter.stmt.InterBlock.interBlock
import interpreter.{Environment, Result}
import parser.stmt.Function


class InterFunction(declaration: Function) extends Callable {

  override def argumentSize: Int =
    declaration.params.size

  override def call(arguments: List[Result]): InterResult[Result] = env => {

    val argsEnv = arguments.zip(declaration.params)
      .foldRight(new Environment(Some(env)))((argParams, e: Environment) => defineValue(e, argParams._2.lexeme, argParams._1))

    (interBlock(declaration.body)(argsEnv)._1, argsEnv.restore())
  }

  private def defineValue(env: Environment, name: String, result: Result): Environment =
    result.value.map(value => env.define(name, value)).getOrElse(env)
}
