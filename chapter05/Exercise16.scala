import scala.{Stream => _, _}

sealed trait Stream[+A] {
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

  def take(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, t), n) => if (n == 0) None else  Some((h(), (t(), n - 1)))
    case _ => None
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

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) => if (p(h())) Some((h(), t())) else None
    case _ => None
  }

  def headOption: Option[A] = foldRight(None:Option[A])((h, t) => Some(h))

  def map[B](f: A => B): Stream[B] = unfold(() => this)(s => s() match {
    case Empty => None
    case Cons(h, t) => Some((f(h()), t))
  })

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C):Stream[C] = unfold((this, s)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
    case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
    case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s).takeWhile {
    case (_, Some(_)) => true
    case _ => false
  }.forAll {
    case (Some(a), Some(b)) => a == b
    case _ => false
  }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => t()
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(h, t) => Some(s -> t())
    case Empty => None
  }.append(Stream(Empty))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = unfold(this) {
    case s@Cons(h, t) => Some(s.foldRight(z)(f) -> t())
    case Empty => None
  }.append(Stream(z))

  def scanRight2[B](z: => B)(f: (A, => B) => B): Stream[B]  = foldRight(Stream(z))((a, s) => cons(f(a, s.headOption.get), s))

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

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }
  // f((s, x)) => 

  val ones: Stream[Int] = unfold(())(_ => Some((1, ())))

  def constant[A](a: A): Stream[A] =  unfold(())(_ => Some(a, ()))

  def from(n: Int): Stream[Int] = unfold(n)(m => Some((m, m+1)))

  def fibs: Stream[Long] = unfold((1, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
}

import Stream._
println(Stream(1,2,3).scanRight(0)(_ + _).toList)
println(ones.scanRight(empty[Int])(cons(_, _)).take(1).map(_.take(2).toList).toList)

println(Stream(1,2,3).scanRight2(0)(_ + _).toList)
println(ones.scanRight2(empty[Int])(cons(_, _)).take(1).map(_.take(2).toList).toList)
