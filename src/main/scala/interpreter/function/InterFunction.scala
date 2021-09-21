package org.compiler.example
package interpreter.function

import interpreter.InterResult.InterResult
import interpreter.stmt.InterBlock.interBlock
import interpreter.{Environment, Result}
import lexer.Token
import parser.stmt.Function


class InterFunction(declaration: Function, closure: Environment) extends Callable {

  override def argumentSize: Int =
    declaration.params.size

  override def call(funName: Token, arguments: List[Result]): InterResult[Result] = env => {

    val envWithClosure: Environment = env.addClosure(closure)

    val envWithArguments = arguments.zip(declaration.params)
      .foldRight(envWithClosure.createLocalEnv)((argParams, e: Environment) => defineValue(e, argParams._2, argParams._1))

    val (funResult, blockEnv) = interBlock(declaration.body)(envWithArguments)
    (funResult, updateFunctionWithClosureEnv(funName.lexeme, blockEnv).getOrElse(env))
  }

  private def updateFunctionWithClosureEnv(funName: String, blockEnv: Environment): Option[Environment] = {
    val funClosureEnv = blockEnv.getClosure(2)
    val funEnv = blockEnv.restoreTo(2)
    funEnv.assign(funName, new InterFunction(declaration, funClosureEnv))
  }

  private def defineValue(env: Environment, token: Token, result: Result): Environment =
    result.value.flatMap(value => env.define(token, value).toOption).getOrElse(env)
}
