object Par {
  type Par[A] = A; //FIXME

  def unit[A](a: => A): Par[A] = a //FIXME
  def fork[A](a: => Par[A]): Par[A] = a //FIXME
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def run[A](a: Par[A]): A = a //FIXME

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
    = Par.unit(f(Par.run(a), Par.run(b))) //FIXME
}

import Par._

def sum(ints: IndexedSeq[Int]): Par[Int] = {
  if (ints.size <= 1)
    Par.unit(ints.headOption.getOrElse(0))
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))((a: Int, b: Int) => a + b)
  }
}

println(Par.run(sum(IndexedSeq(1, 2, 3))))
