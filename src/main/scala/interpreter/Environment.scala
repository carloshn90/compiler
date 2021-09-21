package org.compiler.example
package interpreter

import error.ErrorCompiler
import lexer.Token

import scala.annotation.tailrec

/**
 * Environment save the variable values, where the head is the local scope and the last element is the global scope
 *
 * @param values linkedList of values local -> closure -> global
 */
class Environment(values: List[Map[String, Any]] = List(Map())) {

  def size: Int = values.map(_.size).sum

  def define(token: Token, value: Any): Either[ErrorCompiler, Environment] = {
    if (values.head.contains(token.lexeme)) Left(ErrorCompiler(token.line, s"Already a variable with the name: ${token.lexeme} in this scope."))
    else {
      val localScope: Map[String, Any] = values.head + (token.lexeme -> value)
      Right(new Environment(localScope +: values.tail))
    }
  }

  def assign(name: String, value: Any): Option[Environment] = values
    .find(_.contains(name))
    .map(_ => new Environment(updateValue(name, value, values)))

  def get(name: String): Either[String, Any] = values
    .find((value: Map[String, Any]) => value.contains(name))
    .flatMap((value: Map[String, Any]) => value.get(name))
    .toRight(s"Undefined variable '$name'.")

  def restore: Environment = new Environment(values.tail)

  def restoreTo(index: Int): Environment =
    new Environment(removeByIndex(index, values))

  def createLocalEnv: Environment =
    new Environment(List(Map[String, Any]()) ::: values)

  def addClosure(closure: Environment): Environment =
    new Environment(closure.getValues ::: values)

  def getValues: List[Map[String, Any]] = values

  def getClosure(index: Int): Environment =
    new Environment(getByIndex(index - 1, values, List()))

  private def updateValue(name: String, value: Any, values: List[Map[String, Any]]): List[Map[String, Any]] = values match {
    case List()                   => List()
    case h::t if h.contains(name) => (h + (name -> value)) +: t
    case h::t                     => h +: updateValue(name, value, t)
  }

  @tailrec
  private def removeByIndex(index: Int, values: List[Map[String, Any]]): List[Map[String, Any]] = index match {
    case 0                    => values
    case _ if values.isEmpty  => List()
    case _                    => removeByIndex(index - 1, values.tail)
  }

  @tailrec
  private def getByIndex(index: Int, values: List[Map[String, Any]], acc: List[Map[String, Any]]): List[Map[String, Any]] = index match {
    case 0                    => acc
    case _ if values.isEmpty  => acc
    case _                    => getByIndex(index - 1, values.tail, acc :+ values.head)
  }
}
