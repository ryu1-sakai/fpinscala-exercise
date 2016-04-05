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
}

val rng = SimpleRNG(42)
val (n1, rng2) = SimpleRNG.intDouble(rng)
val (n2, rng3) = SimpleRNG.doubleInt(rng2)
val (n3, rng4) = SimpleRNG.double3(rng3)

println(n1)
println(n2)
println(n3)
