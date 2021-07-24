package org.compiler.example
package error

case class ErrorCompiler(line: Int, message: String) {
  override def toString = s"Error in Line $line: $message"
}

