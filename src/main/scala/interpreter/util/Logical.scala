package org.compiler.example
package interpreter.util

import interpreter.Result

object Logical {

  def checkCondition(cond: Result): Boolean =
    cond.value.exists(isTruthy)

  def isTruthy(value: Any): Boolean = value match {
    case Nil            => false
    case value: Boolean => value
    case _              => true
  }
}
