package org.compiler.example
package util

import cats.Functor


trait Applicative[F[_]] extends Functor[F]{

  def unit[A](a: => A): F[A]

  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

  override def map[A,B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((h, t) => map2(f(h), t)(_ :: _))
}

object Applicative {

   def eitherApplicative[E]: Applicative[({type f[x] =Either[E, x]})#f] =
     new Applicative[({type f[x] = Either[E, x]})#f] {
       override def unit[A](a: => A): Either[E, A] = Right(a)
       override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
     }
}
