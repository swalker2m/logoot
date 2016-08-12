package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz._, Scalaz._

object GenerateLineIdSpec extends Specification with ScalaCheck with Arbitraries {
  /*
  def dump(id: LineId): String =
    id match {
      case LineId.Beginning       =>
        "Beginning"

      case LineId.Middle(posList) =>
        posList.toList.map { pos =>
          f"(${pos.d}%2s, ${pos.s.uuid.toString}, ${pos.t.time}%10d)"
        }.mkString(", ")

      case LineId.End             =>
        "End"
    }
  */

  "GenerateLineId" should {
    "generate requested number of ids in order" in
      forAll { (p: LineId, q: LineId, sid: SiteId, n: Int, seed: Long) =>
        val (p0, q0) = if (p < q) (p, q) else (q, p)

        val count = (n % 999).abs + 1

        val ids = GenerateLineId(p0, q0, count, None).eval(LineIdState(sid, seed))
        val seq = (p0 :: ids) :+ q0

//        println("--------------")
//        val comps = seq.zip(seq.tail).map { case (a,b) => a < b } :+ true
//        comps.zip(seq).foreach { case (c, i) =>
//          println((if (c) "<  " else ">= ") + dump(i))
//        }
//        println("--------------")

        if (p == q)
          ids.size == 0
        else
          (ids.size == count) && seq.zip(seq.tail).forall {
            case (a, b) => a < b
          }
      }
  }
}
