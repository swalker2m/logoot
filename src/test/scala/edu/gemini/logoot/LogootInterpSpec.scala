package edu.gemini.logoot

import edu.gemini.logoot.LogootMessage.Patch
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

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

  case class InsertModule[T](init: List[T], pos: Int, update: List[T]) extends LogootModule[T] {
    val loc: Int = if (init.size == 0) 0 else (pos % init.size).abs

    def test: Logoot[Patch[T]] =
      for {
        _ <- insert(loc, update)
        p <- finishEdit
      } yield p
  }

  case class DeleteModule[T](init: List[T], pos: Int, count: Int) extends LogootModule[T] {
    val loc: Int = if (init.size == 0) 0 else (pos % init.size).abs
    val cnt: Int = (count % 1000).abs

    def test: Logoot[Patch[T]] =
      for {
        _ <- delete(loc, cnt)
        p <- finishEdit
      } yield p
  }

  "LogootInterp" should {
    "insert" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>
        val mod             = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(mod)(mod.test, state0)

        init.patch(mod.loc, update, 0) == state1.doc.values && state1.history.member(patch.pid)
      }

    "delete" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, count: Int) =>
        val mod             = DeleteModule(init, pos, count)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(mod)(mod.test, state0)

        init.patch(mod.loc, Nil, (init.size - mod.loc) min mod.cnt) == state1.doc.values
      }

    "move" in
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

    "undo insert" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val (state2, msg)   = LogootInterp.run(iMod)(iMod.undo(patch.pid), state1)

        init == state2.doc.values && (msg match {
          case Some(LogootMessage.Undo(_, d)) => d == -Degree.One
          case _                              => false
        })
      }

    "undo delete" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, count: Int) =>

        val dMod            = DeleteModule(init, pos, count)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(dMod)(dMod.test, state0)
        val (state2, msg)   = LogootInterp.run(dMod)(dMod.undo(patch.pid), state1)

        init == state2.doc.values && (msg match {
          case Some(LogootMessage.Undo(_, d)) => d == -Degree.One
          case _                              => false
        })
      }

    "skip undo if not necessary" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>
        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val state2          = LogootInterp.exec(iMod)(iMod.undo(pid), state1)
        val (state3, msg)   = LogootInterp.run(iMod)(iMod.undo(pid), state2)

        init == state3.doc.values && (msg match {
          case None => true
          case _    => false
        })
      }

    "undo multiple redone ops" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val two             = Degree(2)

        val state2          = state1.copy(history = state1.history + (pid -> (patch, two)))
        val (state3, msg)   = LogootInterp.run(iMod)(iMod.undo(pid), state2)

        init == state3.doc.values && (msg match {
          case Some(LogootMessage.Undo(_, d)) => d == -two
          case _                              => false
        })
      }

    "redo insert" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val state2          = LogootInterp.exec(iMod)(iMod.undo(pid), state1)
        val (state3, msg)   = LogootInterp.run(iMod)(iMod.redo(pid), state2)

        state1.doc.values == state3.doc.values && (msg match {
          case Some(LogootMessage.Redo(_, Degree.One)) => true
          case _                                       => false
        })
      }

    "skip redo if not necessary" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val state2          = LogootInterp.exec(iMod)(iMod.undo(pid), state1)
        val state3          = LogootInterp.exec(iMod)(iMod.redo(pid), state2)
        val (state4, msg)   = LogootInterp.run(iMod)(iMod.redo(pid), state3)

        state1.doc.values == state4.doc.values && (msg match {
          case None => true
          case _    => false
        })
      }

    "redo multiple undone ops" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val two             = Degree(2)

        val state2          = state1.copy(history = state1.history + (pid -> (patch, -two)))
        val (state3, msg)   = LogootInterp.run(iMod)(iMod.redo(pid), state2)

        state1.doc.values == state3.doc.values && (msg match {
          case Some(LogootMessage.Redo(_, d)) => d == two + Degree.One
          case _                              => false
        })
      }

    "handle remote undo messages" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val undo            = LogootMessage.Undo[Int](pid, -Degree.One)
        val state2          = LogootInterp.exec(iMod)(iMod.receive(undo), state1)

        init == state2.doc.values && (state2.history.lookup(pid) match {
          case Some((_, Degree.Zero)) => true
          case _                      => false
        })
      }

    "handle remote redo messages" in
      forAll { (site0: SiteId, seed0: Long, site1: SiteId, seed1: Long, init: List[Int], pos: Int, update: List[Int]) =>

        val iMod            = InsertModule(init, pos, update)
        val state0          = Initialize(site0, seed0, site1, seed1, init)
        val (state1, patch) = LogootInterp.run(iMod)(iMod.test, state0)
        val pid             = patch.pid
        val state2          = LogootInterp.exec(iMod)(iMod.undo(pid), state1)
        val redo            = LogootMessage.Redo[Int](pid, Degree.One)
        val (state3, msg)   = LogootInterp.run(iMod)(iMod.receive(redo), state2)

        state1.doc.values == state3.doc.values && (state3.history.lookup(pid) match {
          case Some((_, Degree.One)) => true
          case _                     => false
        })
      }
  }
}
