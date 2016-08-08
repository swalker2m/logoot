package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz._, Scalaz._
import scalaz.Ordering.{EQ, GT, LT}

object PositionSpec extends Specification with ScalaCheck with Arbitraries {
  "Position" should {
    "be ordered" !
      forAll { (p0: Position, p1: Position) =>
        Order[Position].order(p0, p1) match {
          case EQ =>
            (p0.d == p1.d) && (p0.s == p1.s) && (p0.t == p1.t)

          case LT =>
            (p0.d < p1.d) || ((p0.d == p1.d) &&
              ((p0.s < p1.s) || ((p0.s == p1.s) && (p0.t < p1.t)))
            )

          case GT =>
            (p0.d > p1.d) || ((p0.d == p1.d) &&
              ((p0.s > p1.s) || ((p0.s == p1.s) && (p0.t > p1.t)))
            )
        }
      }

    val initialState = LogootState(SiteId.Min, 0)

    "advance clock when getting the next position id" !
      forAll { (digits: List[Digit]) =>
        val (state, posList) = Logoot.run(digits.map(Position.next).sequenceU, initialState)

        (state.timestamp === Timestamp(digits.length)) &&
          posList.forall(_.s === SiteId.Min) &&
          posList.map(_.t.time) == posList.zipWithIndex.unzip._2.map(_ + 1) &&
          posList.map(_.d) == digits
      }
  }
}
