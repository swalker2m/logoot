package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz._, Scalaz._

object RngSpec extends Specification with ScalaCheck with Arbitraries {
  "Rng nextNumber" should {
    "generate a number from 0 to n-1" !
      forAll { (n: Number, s: Long) =>
        (n == Number.Zero) || {
          val rng     = Rng(s)
          val (_, n2) = rng.nextNumber(n)
          n2 < n
        }
      }
  }
}
