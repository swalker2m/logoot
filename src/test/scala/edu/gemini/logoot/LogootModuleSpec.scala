package edu.gemini.logoot

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Random
import scalaz.Scalaz._
import scalaz._

object LogootModuleSpec extends Specification with ScalaCheck with Arbitraries {

  case class Site[T](state: LogootState[T], remoteOps: Dequeue[LogootMessage[T]])

  final class Sim[T: Arbitrary] extends LogootModule[T] {

    val localInsert: Logoot[Option[LogootMessage[T]]] =
      for {
        doc <- read
        sz   = doc.size
        loc  = if (sz == 0) -1 else Random.nextInt(sz)
        _   <- insert(loc, arbitrary[List[T]].sample.get)
        p   <- finishEdit
      } yield Some(p)

    val localDelete: Logoot[Option[LogootMessage[T]]] =
      for {
        doc <- read
        sz   = doc.size
        loc  = if (sz == 0) -1 else Random.nextInt(sz)
        cnt  = if (sz == 0)  0 else Random.nextInt(sz - loc)
        _   <- delete(loc, cnt)
        p   <- finishEdit
      } yield Some(p)

    def localDo(s: LogootState[T]): Logoot[Option[LogootMessage[T]]] = {
      val keys  = s.history.keys
      val sz    = keys.size
      if (sz == 0)
        point(None)
      else {
        val index = Random.nextInt(sz)
        val key   = keys(index)
        if (Random.nextBoolean()) undo(key) else redo(key)
      }
    }

    def localEdit(remote: Site[T], localAction: Logoot[Option[LogootMessage[T]]]): Logoot[Dequeue[LogootMessage[T]]] =
      localAction.map { _.fold(remote.remoteOps) { _ +: remote.remoteOps } }

    def remoteEdit(localQ: Dequeue[LogootMessage[T]]): Logoot[Dequeue[LogootMessage[T]]] =
      localQ.unsnoc.cata( { case (m, q2) => receive(m).as(q2) }, point(localQ))

    def update(local: Site[T], remote: Site[T]): Logoot[(Dequeue[LogootMessage[T]], Dequeue[LogootMessage[T]])] =
      Random.nextInt(4) match {
        case 0 => localEdit(remote, localInsert).map(r2 => (local.remoteOps, r2))
        case 1 => localEdit(remote, localDelete).map(r2 => (local.remoteOps, r2))
        case 2 => localEdit(remote, localDo(local.state)).map(r2 => (local.remoteOps, r2))
        case 3 => remoteEdit(local.remoteOps).map(l2 => (l2, remote.remoteOps))
      }

    def init(lines: List[T], lid: LineIdState): LogootDoc[T] =
      LogootInterp.exec(this)(insert(-1, lines), LogootState.init[T](lid)).doc

    def finish(site: Site[T]): LogootDoc[T] = {
      val prog = site.remoteOps.toBackStream.toList.traverseU(receive)
      LogootInterp.exec(this)(prog, site.state).doc
    }
  }

  "Logoot edits" should {
    "be eventually consistent" in
      forAll { (s0: LineIdState, s1: LineIdState, s2: LineIdState, init: List[Int], rounds: Int) =>
        val sim    = new Sim[Int]
        val doc0   = sim.init(init, s0)
        val siteA0 = Site(LogootState.init(doc0, s1), Dequeue.empty[LogootMessage[Int]])
        val siteB0 = Site(LogootState.init(doc0, s2), Dequeue.empty[LogootMessage[Int]])

        val count  = (rounds % 50).abs

        // Perform count edits at both sites, occasionally incorporating edits
        // of the opposite site.  In the end, there will possibly be un-executed
        // operations from the remote opposite to apply to each site.
        val (siteA1, siteB1) = ((siteA0, siteB0)/:(0 until count)) { case ((a1, b1), _) =>
          val (sa, (qa2, qb2)) = LogootInterp.run(sim)(sim.update(a1, b1), a1.state)
          val a2 = Site(sa,       qa2)
          val b2 = Site(b1.state, qb2)
          val (sb, (qb3, qa3)) = LogootInterp.run(sim)(sim.update(b2, a2), b1.state)
          (Site(sa, qa3), Site(sb, qb3))
        }

        // Finish any leftover remote operations that weren't applied and
        // extract the resulting doc.
        val docA = sim.finish(siteA1)
        val docB = sim.finish(siteB1)

        // Documents should match now that all edits at both sites have been
        // applied.
        docA.toList == docB.toList
      }
  }

}
