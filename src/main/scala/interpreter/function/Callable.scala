package org.compiler.example
package interpreter.function

import interpreter.InterResult.InterResult
import interpreter.Result

trait Callable {
  def argumentSize: Int

  def call(arguments: List[Result]): InterResult[Result]
}
