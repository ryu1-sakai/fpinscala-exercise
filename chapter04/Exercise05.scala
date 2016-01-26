def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case Nil => Some(Nil)
  case (x::xs) => for {
    z  <- f(x)
    zs <- traverse(xs)(f)
  } yield z::zs
}
