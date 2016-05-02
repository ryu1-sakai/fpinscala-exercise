trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

type Rand[+A] = RNG => (A, RNG)

object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (if (n == Int.MinValue) 0 else Math.abs(n), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))(rng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, nextRNG) = rng.nextInt
    val (d, nextNextRNG) = double(nextRNG)
    ((n, d), nextNextRNG)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (n, nextRNG) = rng.nextInt
    val (d, nextNextRNG) = double(nextRNG)
    ((d, n), nextNextRNG)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, nextRNG) = double(rng)
    val (d2, nextNextRNG) = double(nextRNG)
    val (d3, nextNextNextRNG) = double(nextNextRNG)
    ((d1, d2, d3), nextNextNextRNG)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(int))(rng)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => fs match {
      case h::t => {
        val (a, rng2) = h(rng)
        val (as, rng3) = sequence(t)(rng2)
        (a::as, rng3)
      }
      case _ => (Nil, rng)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  var count = 0

  def nonNegativeIntLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(i)
      else {
        count += 1
        println(s"hoge $i $count")
        nonNegativeIntLessThan(n)
      }
    }
  }
}


case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State[S, B] {
    s => {
      val (a, s1) = this.run(s)
      (f(a), s1)
    }
  }

  def map2[B](f: (A, A) => B): State[S, B] = State[S, B] {
    s => {
      val (a1, s1) = this.run(s)
      val (a2, s2) = this.run(s1)
      (f(a1, a2), s2)
    }
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] {
    s => {
      val (a, s1) = this.run(s)
      f(a).run(s1)
    }
  }
}

object State {
  def unit[S, A](a: A): State[S, A] = State[S, A] {s => (a, s)}

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = State {
    s => ss match {
      case Nil => (Nil, s)
      case h::t => {
        val (a, s1) = h.run(s)
        val (as, s2) = sequence(t).run(s1)
        (a::as, s2)
      }
    }
  }
}

val randomInt = State[RNG, Int] {r => r.nextInt}
val randomDouble = randomInt.map(x => x / (Int.MaxValue.toDouble + 1))
val randomInt2 = randomInt.map2((x, y) => (x, y))

def randomNonNegativeIntLessThan(n: Int): State[RNG, Int] = randomInt.flatMap {
  x => {
    val mod = x % n
    if (x + (n - 1) - mod >= 0) State.unit(mod)
    else randomNonNegativeIntLessThan(n)
  }
}

def rng2State(seed: Int) = {
  val r = SimpleRNG(seed)
  State[RNG, Int] {r => r.nextInt}
}
val listRandomInt = List(1, 2, 3).map(rng2State(_))
val randomIntList = State.sequence(listRandomInt)

val r0 = SimpleRNG(100)

val (i1, r1) = randomInt.run(r0)
val (i2, r2) = randomInt.run(r1)
println(i1, i2)

val (d1, r3) = randomDouble.run(r2)
val (d2, r4) = randomDouble.run(r3)
println(d1, d2)

val (ii, r5) = randomInt2.run(r4)
println(ii)

val (i3, r6) = randomNonNegativeIntLessThan(200).run(r5)
val (i4, r7) = randomNonNegativeIntLessThan(200).run(r6)
println(i3, i4)
