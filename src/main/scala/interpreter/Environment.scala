package org.compiler.example
package interpreter

class Environment(global: Option[Environment] = None, values: Map[String, Any] = Map()) {

  def size: Int = values.size

  def define(name: String, value: Any): Environment =
    new Environment(global, values + (name -> value))

  def assign(name: String, value: Any): Option[Environment] = getGlobal(name)
    .map(_ => new Environment(assignGlobal(name, value), values))
    .orElse(assignLocal(name, value))

  def get(name: String): Either[String, Any] = getGlobal(name) match {
    case Some(v) => Right(v)
    case _       => getLocal(name)
  }

  def restore(): Environment =
    global.getOrElse(new Environment())

  private def assignLocal(name: String, value: Any): Option[Environment] = values
    .get(name)
    .map(_ => define(name, value))

  private def assignGlobal(name: String, value: Any): Option[Environment] =
    global.flatMap(g => g.assign(name, value))

  private def getLocal(name: String): Either[String, Any] = values.get(name)
    .toRight(s"Undefined variable '$name'.")

  private def getGlobal(name: String): Option[Any] =
    global.flatMap(g => g.get(name) match {
      case Right(value) => Some(value)
      case _            => None
    })
}
