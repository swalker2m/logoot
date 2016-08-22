package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz._, Scalaz._

object GenerateLineIdSpec extends Specification with ScalaCheck with Arbitraries {
  "GenerateLineId" should {
    "generate requested number of ids in order" in
      forAll { (p: LineId, q: LineId, sid: SiteId, n: Int, seed: Long) =>
        val (p0, q0) = if (p < q) (p, q) else (q, p)

        val count = (n % 199).abs + 1

        val ids = GenerateLineId(p0, q0, count, None).eval(LineIdState(sid, seed))
        val seq = (p0 :: ids) :+ q0

        if (p == q)
          ids.size == 0
        else
          (ids.size == count) && seq.zip(seq.tail).forall {
            case (a, b) => a < b
          }
      }
  }
}
