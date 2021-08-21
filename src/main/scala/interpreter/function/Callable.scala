package org.compiler.example
package interpreter.function

trait Callable {
  def argumentSize: Int

  def call(arguments: List[Any]): Either[String, Any]
}
