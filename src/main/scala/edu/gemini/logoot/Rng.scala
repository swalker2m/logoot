package edu.gemini.logoot

import scala.annotation.tailrec
import scalaz._, Scalaz._

/** Random number generator from Functional Programming in Scala, modified to
  * compute random `Number`s.
  */
trait Rng {

  /**
   * Computes the next random integer and a new `Rng` to use for the next init.
   */
  def nextInt: (Rng, Int)

  /** Computes the next random integer in the range (0, bound] and a new `Rng`
    * to use for the next random integer.
    */
  def nextInt(bound: Int): (Rng, Int) = {
    val (nextRng, n) = nextInt
    (nextRng, (n % bound).abs)
  }

  /** Computes a random `Number` from (0, bound-1].  This method allows
    * computing arbitrarily long `Numbers`, not limited by the size of an int.
    */
  def nextNumber(bound: Number): (Rng, Number) = {
    @tailrec
    def go(rem: List[Digit], limit: Boolean, rng: Rng, res: List[Digit]): (Rng, Number) =
      rem match {
        case Nil    =>
          (rng, Number(res.reverse))

        case h :: t =>
          // Compute the next digit.  If we are, so far, potentially creating a
          // number equal to (max - 1), then the digit can only range from 0 to
          // whatever the corresponding digit in (max - 1) inclusive.  Otherwise
          // it can range from (0, Digit.Base].
          val m      = if (limit) h.toInt + 1 else Digit.Base
          val (r, i) = rng.nextInt(m)

          // Determine whether the next digit should be limited as well.
          val l      = limit && (i === h.toInt)

          // Move on to the following digit, building up the result in reverse.
          go(t, l, r, Digit.fromInt(i) :: res)
      }

    go(bound.decr.toDigits, limit = true, this, Nil)
  }
}

object Rng {
  def apply(seed: Long): Rng =
    new Rng {
      def nextInt: (Rng, Int) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        (apply(newSeed), (newSeed >>> 16).toInt)
      }
    }
}
