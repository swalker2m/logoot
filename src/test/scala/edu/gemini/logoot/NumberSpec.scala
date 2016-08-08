package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz._, Scalaz._
import scalaz.Ordering.{EQ, GT, LT}

object NumberSpec extends Specification with ScalaCheck with Arbitraries {
  "Number" should {
    "convert to/from base-10" !
      forAll { (bi: BigInt) =>
        val bi2 = if (bi < BigInt(0)) BigInt(0) else bi
        Number.fromBase10(bi).toBase10 == bi2
      }

    "trim leading zeros" !
      forAll { (ds: List[Digit]) =>
          Number(ds).toDigits match {
          case Nil     => false
          case List(_) => true
          case h :: _  => h.toInt > 0
        }
      }

    "be ordered" !
      forAll { (n0: Number, n1: Number) =>
        val b0 = n0.toBase10
        val b1 = n1.toBase10

        Order[Number].order(n0, n1) match {
          case EQ => b0 == b1
          case LT => b0 <  b1
          case GT => b0 >  b1
        }
      }

    "commute under addition" !
      forAll { (n0: Number, n1: Number) =>
        (n0 + n1) === (n1 + n0)
      }

    "obey the monoid laws" !
      forAll { (n0: Number, n1: Number, n2: Number) =>
        ((n0 |+| Number.Zero) === n0) &&
          ((Number.Zero |+| n0) === n0) &&
          (((n0 |+| n1) |+| n2) === (n0 |+| (n1 |+| n2)))
      }

    "never be negative" !
      forAll { (n0: Number, n1: Number) =>
        (n0 > n1) || ((n0 - n1) === Number.Zero)
      }
  }
}
