package org.compiler.example
package interpreter

object PrettierOutput {

  def stringify(value: Any): String = value match {
    case Nil               => "Nil"
    case d: Double         => stringifyDouble(d)
    case v                 => v.toString
  }

  private def stringifyDouble(double: Double): String = {
    val doubleStr = double.toString
    if (doubleStr.endsWith(".0")) doubleStr.substring(0, doubleStr.length - 2)
    else doubleStr
  }
}
