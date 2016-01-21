object Main {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (x => (b map (y => f(x, y))))

  def sequence[A](oas: List[Option[A]]): Option[List[A]] = oas match {
    case Nil => Some(Nil)
    case (x::xs) => map2(x, sequence(xs))(_ :: _)
  }

  def main(args: Array[String]): Unit = {
    val loOk = List(Option(1), Option(2), Option(3))
    val loNg = List(Option(1), None, Option(3))
    println(sequence(loOk))
    println(sequence(loNg))
  }
}
