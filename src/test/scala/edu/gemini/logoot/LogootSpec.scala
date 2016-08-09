package edu.gemini.logoot

import edu.gemini.logoot.LogootOp.{Delete, Insert, Patch}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Random
import scalaz._

object LogootSpec extends Specification with ScalaCheck with Arbitraries {

  def toOp[A](ops: List[LogootOp[A]]): LogootOp[A] =
    ops match {
      case List(op) => op
      case _        => Patch(ops)
    }

  def insertionPoint[A](doc: LogootDoc[A]): (LineId, LineId) = {
    val size = doc.size
    val index = Random.nextInt(size + 1)
    val p = doc.elemAt(index - 1).map(_._1).getOrElse(LineId.Beginning)
    val q = doc.elemAt(index).map(_._1).getOrElse(LineId.End)
    (p, q)
  }

  def simLocalInsert[A : Arbitrary](doc: LogootDoc[A]): Logoot[LogootOp[A]] = {
    // Pick an insertion point.
    val (p, q) = insertionPoint(doc)

    // Pick a number of items to insert.
    val items = arbitrary[List[A]].sample.get
    GenerateLineId(p, q, items.length, None).map { ids =>
      toOp(ids.zip(items).map { case (id0, item) => Insert(id0, item) })
    }
  }

  def simLocalDelete[A](doc: LogootDoc[A]): Logoot[LogootOp[A]] =
    Logoot.point(toOp(doc.fold(List.empty[LogootOp[A]]) { (id, a, lst) =>
      if (Random.nextBoolean()) lst
      else Delete[A](id) :: lst
    }))

  def sim[A: Arbitrary](local: Site[A], remote: Site[A]): (Site[A], Site[A]) =
    if (Random.nextBoolean()) {
      val op = if (Random.nextBoolean()) simLocalInsert(local.doc) else simLocalDelete(local.doc)
      local.doLocal(op, remote)
    } else {
      (local.doRemote(), remote)
    }

  case class Site[A](state: LogootState, doc: LogootDoc[A], remoteOps: Dequeue[LogootOp[A]]) {
    def doLocal(op: Logoot[LogootOp[A]], remote: Site[A]): (Site[A], Site[A]) = {
      val (state2, o) = Logoot.run(op, state)
      val local2      = Site(state2, o(doc), remoteOps)
      val remote2     = remote.copy(remoteOps = o +: remote.remoteOps)
      (local2, remote2)
    }

    def doRemote(): Site[A] =
      remoteOps.unsnoc.cata({ case (op, d) => Site(state, op(doc), d) }, this)
  }

  "Logoot edits" should {
    "be eventually consistent" in
      forAll { (s0: LogootState, s1: LogootState, s2: LogootState, init: List[Int], rounds: Int) =>
        val lines  = Logoot.eval(GenerateLineId(LineId.Beginning, LineId.End, init.size, None), s0)
        val doc0   = ==>>.fromList(lines.zip(init))

        val siteA0 = Site(s1, doc0, Dequeue.empty[LogootOp[Int]])
        val siteB0 = Site(s2, doc0, Dequeue.empty[LogootOp[Int]])

        val count  = (rounds % 50).abs

        // Perform count edits at both sites, occasionally incorporating edits
        // of the opposite site.  In the end, there will possibly be unexecuted
        // operations from the remote opposite to apply to each site.
        val (siteA1, siteB1) = ((siteA0, siteB0)/:(0 to count)) { case ((a1, b1), _) =>
          val (a2, b2) = sim(a1, b1)
          sim(b2, a2).swap
        }

        // Finish any leftover steps and extract the resulting doc.
        def finish[A](site: Site[A]): LogootDoc[A] =
          if (site.remoteOps.isEmpty) site.doc
          else finish(site.doRemote())

        val docA = finish(siteA1)
        val docB = finish(siteB1)

        // Documents should match now that all edits at both sites have been
        // applied.
        docA.toList == docB.toList
      }
  }
}
