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

object SimpleRNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (if (n == Int.MinValue) 0 else Math.abs(n), nextRNG)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRNG)
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

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (i,  r1) = rng.nextInt
      val (is, r2) = ints(count - 1)(r1)
      (i::is, r2)
    }
  }
}

val rng = SimpleRNG(42)

val (xs, r2) = SimpleRNG.ints(100)(rng)
println(xs.length)
println(xs)

