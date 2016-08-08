package edu.gemini.logoot

import edu.gemini.logoot.LineId.{Beginning, Middle, End}
import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Ordering.{EQ, GT, LT}
import scalaz.Scalaz._
import scalaz._

object LineIdSpec extends Specification with ScalaCheck with Arbitraries {
  "LineId" should {
    "order middle positions" !
      forAll { (id0: Middle, id1: Middle) =>
        val z = id0.positions.toList.zipAll(id1.positions.toList, Position.Min, Position.Min).dropWhile {
          case (a, b) => a == b
        }

        Order[Middle].order(id0, id1) match {
          case EQ => z.isEmpty
          case LT => z.headOption.exists { case (a, b) => a < b }
          case GT => z.headOption.exists { case (a, b) => a > b }
        }
      }

    "sort Beginning ahead of everything else" !
      forAll { (id: LineId) =>
        Order[LineId].order(Beginning, id) match {
          case EQ => id == Beginning
          case LT => true
          case GT => false
        }
      }

    "sort End after everything else" !
      forAll { (id: LineId) =>
        Order[LineId].order(id, End) match {
          case EQ => id == End
          case LT => true
          case GT => false
        }
      }
  }
}
