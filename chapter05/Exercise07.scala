import scala.{Stream => _, _}

trait Stream[+A] {
  import Stream._

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

  def takeWhile(p: A => Boolean): Stream[A] = foldRight[Stream[A]](empty)((h, t) => if (p(h)) cons(h, t) else empty)

  def headOption: Option[A] = foldRight(None:Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))
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

println(cons(1, cons(2, empty)).map(_ + 0.1).toList)
println(empty[Int].map(_ + 0.1))

println(cons(1, cons(2, cons(1, empty))).filter(_ < 2).toList)
println(empty[Int].filter(_ < 2))

println(cons(1, cons(2, empty)).append(cons(3, cons(4, empty))).toList)
println(cons(1, cons(2, empty)).append({println("XXX"); cons(3, cons(4, empty))}).takeWhile(_ < 2).toList)

println( cons(1, cons(2, cons(3, empty))).flatMap(x => cons(x, cons(x, empty))).toList )
