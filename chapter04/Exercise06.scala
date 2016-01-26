
sealed trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B] = {
    this match {
      case MyRight(a) => MyRight(f(a))
      case MyLeft(e) => MyLeft(e)
    }
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = {
    this match {
      case MyRight(a) => f(a)
      case MyLeft(e) => MyLeft(e)
    }
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = {
    this match {
      case MyRight(a) => MyRight(a)
      case MyLeft(_) => b
    }
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = {
    for {
      x <- this
      y <- b
    } yield f(x, y)
  }
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
case class MyRight[+A](value: A) extends MyEither[Nothing, A]

println(MyRight(10).map(_ + 20))
println(MyLeft(10).map(_.toString))

println(MyRight(10) flatMap (x => MyRight(x + 11)))
println(MyLeft(10) flatMap (x => MyRight(x)))

println(MyRight(10) orElse MyRight(20))
println(MyLeft(10) orElse MyRight(20))

println(MyRight(10).map2(MyRight(20))(_ + _))
println(MyLeft(10).map2(MyRight(20))((a, b) => ()))
