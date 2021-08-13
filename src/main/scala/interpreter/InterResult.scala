package org.compiler.example
package interpreter

import error.ErrorCompiler

import cats.implicits.catsSyntaxApply

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
  }

  def unit[A](value: Either[ErrorCompiler, A]): InterResult[A] = environment => (value, environment)
}
