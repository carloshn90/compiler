package org.compiler.example
package interpreter.`class`

import interpreter.InterResult.{InterResult, unit}
import interpreter.function.{Callable, InterFunction}
import interpreter.{InterpreterState, Result}
import lexer.Token

class InterClass(name: String, methods: Map[String, InterFunction]) extends Callable {

  override def argumentSize: Int = methods.get(name)
    .map(_.argumentSize)
    .getOrElse(0)

  override def call(funName: Token, arguments: List[Result]): InterResult[Result] = {
    val interInstance: InterInstance = new InterInstance(methods)
    executeConstructor(funName, arguments, interInstance)
  }

  def getName: String = name

  def getMethods: Map[String, Any] = methods

  override def toString = s"InterClass $name"

  private def executeConstructor(funName: Token, arguments: List[Result], instance: InterInstance): InterResult[Result] = instance.get(funName) match {
    case Right(fun) => callFunction(funName, arguments, instance, fun)
    case _          => unit(Right(InterpreterState(List(), Some(instance))))
  }

  private def callFunction(funName: Token, arguments: List[Result], instance: InterInstance, fun: InterFunction): InterResult[Result] = env => {
    val (result, funEnv) = fun.call(funName, arguments)(env)
    unit(result.map(r => InterpreterState(r.printList, Some(instance))))(funEnv)
  }

}
