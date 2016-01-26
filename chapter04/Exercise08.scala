// Exercise 4.8 is only discussion. We wrote 'map2a' for the discussion.

import scala.{Either => _, Right => _, Left => _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(a) => Right(f(a))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      x <- this
      y <- b
    } yield f(x, y)
  }

  def map2a[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[String, C] = (this, b) match {
    case (Left(e), Left(ee)) => Left(e.toString + ee.toString)
    case (Left(e), _) => Left(e.toString)
    case (_, Left(e)) => Left(e.toString)
    case (Right(x), Right(y)) => Right(f(x, y))
  }
}

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case Left(e)::_ => Left(e)
    case Right(x)::xs => sequence(xs) map(xxs => x::xxs)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x::xs => f(x) match {
      case Left(e) => Left(e)
      case Right(b) => traverse(xs)(f) map(bbs => b::bbs)
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

import Either._
