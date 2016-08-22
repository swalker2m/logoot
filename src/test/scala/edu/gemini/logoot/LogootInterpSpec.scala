package edu.gemini.logoot

import edu.gemini.logoot.LogootMessage.Patch
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scalaz.Scalaz._
import scalaz._

object LogootInterpSpec extends Specification with ScalaCheck with Arbitraries {
  def dump[A](lst: List[A]): String =
    lst.mkString("[", ", ", "]")

  // Utility for setting up the initial test state which has the document loaded
  // with ids from a remote peer.
  object Initialize extends LogootModule[Int] {
    def apply(site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, lst: List[Int]): LogootState[Int] = {
      val doc = LogootInterp.exec[Int, Unit](this)(insert(0, lst), LogootState.init(site0, seed0)).doc
      LogootState.init(doc, LineIdState(site1, seed1))
    }
  }

  "LogootInterp" should {
    "insert correctly" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val loc = if (init.size == 0) 0 else (pos % init.size).abs

        object Editor extends LogootModule[Int] {
          def test: Logoot[Patch[Int]] =
            for {
              _ <- insert(loc, update)
              p <- finishEdit
            } yield p
        }

        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(Editor)(Editor.test, state0)

        init.patch(loc, update, 0) == state1.doc.values
      }

    "delete correctly" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, count: Int) =>

        val cnt = (count % 1000).abs
        val loc = if (init.size == 0) 0 else (pos % init.size).abs

        object Editor extends LogootModule[Int] {
          def test: Logoot[Patch[Int]] =
            for {
              _ <- delete(loc, cnt)
              p <- finishEdit
            } yield p
        }

        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(Editor)(Editor.test, state0)

        init.patch(loc, Nil, (init.size - loc) min cnt) == state1.doc.values
      }

    "move correctly" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, count: Int) =>

        val cnt = (count % 1000).abs
        val loc = if (init.size == 0) 0 else (pos % init.size).abs

        object Editor extends LogootModule[Int] {
          def test: Logoot[Patch[Int]] =
            for {
              d  <- read
              is  = d.drop(loc).take(cnt)
              _  <- delete(loc, cnt)
              _  <- insert(0, is)
              p  <- finishEdit
            } yield p
        }

        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(Editor)(Editor.test, state0)

        val is = init.drop(loc).take(cnt)
        val ex = init.patch(loc, Nil, (init.size - loc) min cnt).patch(0, is, 0)

        ex == state1.doc.values
      }
  }

}