package org.compiler.example
package interpreter

import error.ErrorCompiler

import cats.implicits.catsSyntaxApply

sealed trait Result {
  def printList: List[String]
  def value: Option[Any]
}
case class InterpreterState(printList: List[String], value: Option[Any]) extends Result
case class ReturnResult(printList: List[String], value: Option[Any]) extends Result

object InterResult {

  type InterResult[A] = Environment => (Either[ErrorCompiler, A], Environment)

  implicit class InterResultMonad[A](val a: InterResult[A]) {

    def map[B](f: A => B): InterResult[B] = environment => {
      val (expr, env) = a(environment)
      unit(expr.map(f))(env)
    }

    def flatMap[B](f: A => InterResult[B]): InterResult[B] = environment => {
      val (expr, env) = a(environment)
      expr match {
        case Right(v) => f(v)(env)
        case Left(e)  => unit(Left(e))(env)
      }
    }

    def map2[B, C](b: InterResult[B])(f: (A, B) => C): InterResult[C] = environment => {
      val (exprA, envA) = a(environment)
      val (exprB, envB) = b(envA)
      unit(exprA.map2(exprB)(f))(envB)
    }

    def flatMap2[B, C](b: InterResult[B])(f: (A, B) => InterResult[C]): InterResult[C] =
      a.flatMap(aa => b.flatMap(bb => f(aa, bb)))
  }

  def unit[A](value: Either[ErrorCompiler, A]): InterResult[A] = environment => (value, environment)

  def map(a: InterResult[Result])(f: InterResult[Any] => InterResult[Any]): InterResult[Result] = a.flatMap(ra => {
    val valueResult: InterResult[Any] = f(getValue(ra.value))
    valueResult.map(value => InterpreterState(ra.printList, Some(value)))
  })

  def flatMap(a: InterResult[Result])(f: InterResult[Any] => InterResult[Result]): InterResult[Result] = a.flatMap(ra => {
    val valueResult: InterResult[Result] = f(getValue(ra.value))
    valueResult.map(rb => InterpreterState(ra.printList ::: rb.printList, rb.value))
  })

  def map2(a: InterResult[Result], b: InterResult[Result])
                  (f: (InterResult[Any], InterResult[Any]) => InterResult[Any]): InterResult[Result] = a.flatMap2(b)((ra, rb) => {
    val valueResult: InterResult[Any] = f(getValue(ra.value), getValue(rb.value))
    valueResult.map(value => InterpreterState(ra.printList ::: rb.printList, Some(value)))
  })

  private def getValue(valueOption: Option[Any]): InterResult[Any] = valueOption match {
    case Some(value) => unit(Right(value))
    case None        => unit(Left(ErrorCompiler(-1, "Unexpected error")))
  }
}
