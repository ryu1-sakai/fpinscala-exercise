object Par {
  type Par[A] = A; //FIXME

  def unit[A](a: => A): Par[A] = a //FIXME
  def get[A](a: Par[A]): A = a //FIXME

  def map2[A, B, C](a: A, b: B)(f: (A, B) => C) = f(a, b) //FIXME
}

import Par._

def sum(ints: IndexedSeq[Int]): Int = {
  if (ints.size <= 1)
    ints.headOption.getOrElse(0)
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(sum(l), sum(r))(_ + _)
  }
}

println(sum(IndexedSeq(1, 2, 3)))
