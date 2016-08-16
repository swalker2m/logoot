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

  case class Site[T](state: LogootState[T], remoteOps: Dequeue[LogootOp[T]])

  final class Sim[T: Arbitrary] extends LogootModule[T] {

    val localInsert: Logoot[LogootOp[T]] =
      for {
        doc  <- read
        sz    = doc.size
        loc   = if (sz == 0) -1 else Random.nextInt(sz)
        op   <- insert(loc, arbitrary[List[T]].sample.get)
      } yield op

    val localDelete: Logoot[LogootOp[T]] =
      for {
        doc <- read
        sz   = doc.size
        loc  = if (sz == 0) -1 else Random.nextInt(sz)
        cnt  = if (sz == 0)  0 else Random.nextInt(sz - loc)
        op  <- delete(loc, cnt)
      } yield op

    def localEdit(remoteQ: Dequeue[LogootOp[T]], localAction: Logoot[LogootOp[T]]): Logoot[Dequeue[LogootOp[T]]] =
      localAction.map { _ +: remoteQ }

    def remoteEdit(localQ: Dequeue[LogootOp[T]]): Logoot[Dequeue[LogootOp[T]]] =
      localQ.unsnoc.cata( { case (op, q2) => applyOp(op).as(q2) }, point(localQ))

    def update(localQ: Dequeue[LogootOp[T]], remoteQ: Dequeue[LogootOp[T]]): Logoot[(Dequeue[LogootOp[T]], Dequeue[LogootOp[T]])] =
      Random.nextInt(3) match {
        case 0 => localEdit(remoteQ, localInsert).map(r2 => (localQ, r2))
        case 1 => localEdit(remoteQ, localDelete).map(r2 => (localQ, r2))
        case 2 => remoteEdit(localQ).map(l2 => (l2, remoteQ))
      }

    def init(lines: List[T], lid: LineIdState): LogootDoc[T] =
      LogootInterp.exec(this)(insert(-1, lines), LogootState.init[T](lid)).doc

    def finish(site: Site[T]): LogootDoc[T] = {
      val prog = site.remoteOps.toBackStream.toList.traverseU(applyOp)
      LogootInterp.exec(this)(prog, site.state).doc
    }
  }

  "Logoot edits" should {
    "be eventually consistent" in
      forAll { (s0: LineIdState, s1: LineIdState, s2: LineIdState, init: List[Int], rounds: Int) =>
        val sim    = new Sim[Int]
        val doc0   = sim.init(init, s0)
        val siteA0 = Site(LogootState(doc0, s1), Dequeue.empty[LogootOp[Int]])
        val siteB0 = Site(LogootState(doc0, s2), Dequeue.empty[LogootOp[Int]])

        val count  = (rounds % 50).abs

        // Perform count edits at both sites, occasionally incorporating edits
        // of the opposite site.  In the end, there will possibly be un-executed
        // operations from the remote opposite to apply to each site.
        val (siteA1, siteB1) = ((siteA0, siteB0)/:(0 until count)) { case ((a1, b1), i) =>
          val (sa, (qa2, qb2)) = LogootInterp.run(sim)(sim.update(a1.remoteOps, b1.remoteOps), a1.state)
          val (sb, (qb3, qa3)) = LogootInterp.run(sim)(sim.update(qb2,          qa2         ), b1.state)
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
