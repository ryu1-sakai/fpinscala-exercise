import scala.{Stream => _, _}

trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll2(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def drop(n: Int): Stream[A] = {
    if (n <= 0) {
      this
    } else {
      this match {
        case Empty => Empty
        case Cons(h, t) => t().drop(n-1)
      }
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      lazy val hv = h()
      if (p(hv)) Cons(() => hv, () => t().takeWhile(p)) else Empty

  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

import Stream._

println(cons(1, cons({ println("WOW"); 2 }, cons(3, cons(-1, empty)))).forAll(i => i > 0))
println(cons(-1, cons({ println("WOW"); 2}, cons(3, empty))).forAll(i => i > 0))


println(cons(1, cons({ println("WOW"); 2 }, cons(3, cons(-1, empty)))).forAll2(i => i > 0))
println(cons(-1, cons({ println("WOW"); 2}, cons(3, empty))).forAll2(i => i > 0))
