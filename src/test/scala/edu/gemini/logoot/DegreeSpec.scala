package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Order
import scalaz.Ordering.{GT, EQ, LT}

object DegreeSpec extends Specification with ScalaCheck with Arbitraries {
  "Degree" should {
    "increment by one" !
      forAll { (d: Degree) =>
        d.incr == Degree(d.toInt + 1)
      }

    "decrement by one" !
      forAll { (d: Degree) =>
        d.decr == Degree(d.toInt - 1)
      }

    "sum" !
      forAll { (d0: Degree, d1: Degree) =>
        d0 + d1 == Degree(d0.toInt + d1.toInt)
      }

    "subtract" !
      forAll { (d0: Degree, d1: Degree) =>
        d0 - d1 == Degree(d0.toInt - d1.toInt)
      }

    "negate" !
      forAll { (d: Degree) =>
        -d == Degree(-d.toInt)
      }

    "be visible if non-zero positive" !
      forAll { (d: Degree) =>
        d.isVisible == (d.toInt > 0)
      }

    "be hidden if zero or negative" !
      forAll { (d: Degree) =>
        d.isHidden == (d.toInt <= 0)
      }

    "be ordered" !
      forAll { (d0: Degree, d1: Degree) =>
        Order[Degree].order(d0, d1) match {
          case LT => d0.toInt <  d1.toInt
          case EQ => d0.toInt == d1.toInt
          case GT => d0.toInt >  d1.toInt
        }
      }
  }

}
