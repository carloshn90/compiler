package org.compiler.example
package interpreter.function

import error.ErrorCompiler
import interpreter.Environment
import interpreter.stmt.InterBlock.interBlock
import parser.stmt.Function


class InterFunction(declaration: Function) extends Callable {

  override def argumentSize: Int =
    declaration.params.size

  override def call(arguments: List[Any]): Either[ErrorCompiler, List[String]] = {

    val env = arguments.zip(declaration.params)
      .foldRight(new Environment())((argParams, e: Environment) => e.define(argParams._2.lexeme, argParams._1))

    interBlock(declaration.body)(env)._1
  }
}
