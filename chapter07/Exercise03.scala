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

  private case class BinaryFuture[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    def isDone = a.isDone && b.isDone
    def get = f(a.get, b.get)
    def get(timeout: Long, units: TimeUnit) = {
      val startNanos = System.nanoTime
      val av = try {
        a.get(timeout, units)
      } catch {
        case e: Throwable => {
          b.cancel(true)
          throw e
        }
      }
      val elapsedNanos = System.nanoTime - startNanos
      val restNanos = units.toNanos(timeout) - elapsedNanos
      val bv = b.get(restNanos, TimeUnit.NANOSECONDS)
      f(av, bv)
    }
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean) = {
      if (!a.cancel(evenIfRunning) && !a.isDone) false
      else b.cancel(evenIfRunning)
    }
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
