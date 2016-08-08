package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Order
import scalaz.Ordering.{EQ, GT, LT}

object DigitSpec extends Specification with ScalaCheck with Arbitraries {
  "Digit" should {
    "be ordered" !
      forAll { (d0: Digit, d1: Digit) =>
        val i0 = d0.toInt
        val i1 = d1.toInt

        Order[Digit].order(d0, d1) match {
          case EQ => i0 == i1
          case LT => i0 <  i1
          case GT => i0 >  i1
        }
      }

    "be in the range (0, Digit.Base]" !
      forAll { (i: Int) =>
        val i0 = Digit.fromInt(i).toInt
        (i0 >= 0) && (i0 < Digit.Base)
      }
  }
}
