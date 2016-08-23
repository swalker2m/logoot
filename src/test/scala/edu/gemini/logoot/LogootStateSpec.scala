package edu.gemini.logoot

import edu.gemini.logoot.LogootMessage.Patch
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.==>>

object LogootStateSpec extends Specification with ScalaCheck with Arbitraries {

  "LogootState" should {
    "have a working history buffer lens" in
      forAll { (site: SiteId, seed: Long) =>
        val state0 = LogootState.init[Int](site, seed)

        val pid    = PatchId.random
        val p      = Patch[Int](pid, Nil)
        val hb     = ==>>(pid -> (p, Degree.One))

        val two    = Degree(2)
        val hlens  = LogootState.history[Int]
        val plens  = LogootState.patch[Int](pid)

        val state1 = hlens.set(state0, hb)
        val deg1   = plens.get(state1).map(_._2)

        val state2 = plens.set(state1, (p, two))
        val deg2   = plens.get(state2.get).map(_._2)

        state1.history.member(pid) &&
          deg1.contains(Degree.One) &&
          deg2.contains(two)
      }
  }
}
