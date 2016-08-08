package edu.gemini.logoot

import scalaz._, Scalaz._

/** An arbitrary length unsigned number in base `Digit.Base`. */
sealed trait Number {

  /** Gets the individual digits that make up this number. */
  def toDigits: List[Digit]

  /** Gets the number of digits in this Number. */
  def width: Int = toDigits.length

  /** Calculates the base-10 `BigInt` equivalent of this `Number`. */
  def toBase10: BigInt =
    (toDigits:\(BigInt(0), BigInt(1))) { case (d, (acc, pow)) =>
      (acc + BigInt(d.toInt) * pow, pow * Digit.Base)
    }._1

  /** The count of digits that make up this number. */
  def size: Int =
    toDigits.size

  def +(n: Number): Number =
    Number.fromBase10(toBase10 + n.toBase10)

  def incr: Number =
    this + Number.One

  def -(n: Number): Number =
    Number.fromBase10(toBase10 - n.toBase10)

  def decr: Number =
    this - Number.One

  override def equals(a: Any): Boolean =
    a match {
      case n: Number => toDigits === n.toDigits
      case _         => false
    }

  override def hashCode: Int =
    toDigits.hashCode()

  override def toString: String =
    toDigits.map(_.toInt).mkString(".")
}

object Number {
  val Zero: Number =
    Number(Digit.Zero)

  val One: Number =
    Number(Digit.One)

  def apply(ds: Digit*): Number =
    Number(ds.toList)

  def apply(ds: List[Digit]): Number =
    new Number {
      val toDigits: List[Digit] =
        ds.dropWhile(_.toInt === 0) match {
          case Nil => List(Digit.Zero)
          case x   => x
        }
    }

  /** Calculates the `Number` in `Digit.Base` that corresponds to the given
    * base-10 `BitInt`.
    *
    * If `bi` is negative, the result is 0.
    */
  def fromBase10(bi: BigInt): Number = {
    val zero = BigInt(0)

    def go(rem: BigInt, res: List[Digit]): List[Digit] = {
      val (a, b) = rem /% Digit.Base
      val res0   = Digit.fromInt(b.toInt) :: res
      if (a == zero) res0 else go(a, res0)
    }

    if (bi < zero) Zero else Number(go(bi.abs, Nil))
  }

  implicit val OrderNumber: Order[Number] =
    Order.orderBy(_.toBase10)

  implicit val MonoidNumber: Monoid[Number] =
    Monoid.instance((a, b) => a + b, Zero)
}
