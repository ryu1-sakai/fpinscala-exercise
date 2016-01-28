import scala.{Stream => _, _}

trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }


  def take(n: Int): Stream[A] = {
    if (n <= 0) {
      Empty
    } else {
      this match {
        case Empty => Empty
        case Cons(h, t) => Cons(h, () => t().take(n-1))
      }
    }
  }

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

println(Cons(() => { println("Hello"); 1 }, () => cons(2, empty)).toList)

println(cons(1, cons(2, Cons(() => { println("FFFFFFFFF"); 3 }, () => empty))).take(2).toList)

println(Cons(() => { println("!!!!!!!!"); 1 },() => cons(2, cons(3, empty))).drop(2).toList)

println(Cons(() => { println("22222"); -2 }, () => cons(-1, cons(0, cons(1, empty)))).takeWhile(_ <= 0).toList)

println(cons(1, cons(2, Cons(() => { println("FFFFFFFFF"); 3 }, () => { println("EMPTY"); Empty }))).take(3))
