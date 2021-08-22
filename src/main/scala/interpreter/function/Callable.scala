package org.compiler.example
package interpreter.function

import error.ErrorCompiler

trait Callable {
  def argumentSize: Int

  def call(arguments: List[Any]): Either[ErrorCompiler, List[String]]
}
