import java.util.concurrent.Callable
import java.util.concurrent.ExecutorService
import java.util.concurrent.Future
import java.util.concurrent.TimeUnit

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: => A): Par[A] = _ => UnitFuture(a)


  def fork[A](a: => Par[A]): Par[A] = {
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
  }
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es => UnitFuture(f(a(es).get, b(es).get))
  }
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
