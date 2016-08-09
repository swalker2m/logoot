package edu.gemini.logoot

import LineId.{Beginning, Middle, End}

import scala.annotation.tailrec
import scalaz._, Scalaz._

/** Implements an algorithm for generating new unique ids that fall between two
  * other ids.
  */
private[logoot] object GenerateLineId {

  def apply(p: LineId, q: LineId, n: Int, boundary: Option[Int]): Logoot[List[LineId]] = {
    def qIsZero = q match {
      case Middle(posList) => posList.toList.all(_.d === Digit.Zero)
      case _               => false
    }

    // Verify that p < q and that q is not a zero position (otherwise there is
    // no space between p and q and we return an empty list.)
    if ((p >= q) || (n < 1) || qIsZero)
      Logoot.point(List.empty[LineId])
    else
      gen(p, q, n, boundary.filter(_ > 0))
  }

  private val PosZero = Position(Digit.Zero, SiteId.Min, Timestamp.Zero)
  private val PosOne  = Position(Digit.One,  SiteId.Min, Timestamp.Zero)
  private val PosMax  = Position(Digit.Max,  SiteId.Max, Timestamp.Max)

  private def gen(p: LineId, q: LineId, n: Int, boundary: Option[Int]): Logoot[List[LineId]] = {

    // Turns an ID into an infinite stream of positions.
    def posStream(id: LineId): Stream[Position] =
      id match {
        case Beginning   =>                   Stream.continually(PosZero)
        case Middle(pos) => pos.toStream #::: Stream.continually(PosZero)
        case End         =>                   Stream.continually(PosMax)
      }

    //
    // PosZipper is vaguely like a list Zipper in that it stores the prefix of
    // the current location in reverse, but it introduces a "phantom" position
    // element to make the math work out when computing the distance between
    // two positions.  For example, given base-10
    //
    // p = <5.2.1><6.2.1> => PosZipper([<5.2.1>], <0.0.0>, [<6.2.1>])
    // q = <5.3.1><4.3.1> => PosZipper([<5.3.1>], <1.0.0>, [<4.3.1>])
    //
    // we can compute 7 positions of length 2 between p and q
    //
    // <5.2.1><7.x.x>, <5.2.1><8.x.x>, <5.2.1><9.x.x>,
    // <5.3.1><0.x.x>, <5.3.1><1.x.x>, <5.3.1><2.x.x>, <5.3.1><3.x.x>
    //
    // q.prefix(2) = 1.4
    // p.prefix(2) = 0.6
    //
    // 14 - 6 - 1 = 7
    //
    class PosZipper(val revPrefix: List[Position], val phantom: Position, val suffix: Stream[Position]) {
      def prefixDigits(index: Int): Stream[Digit] =
        phantom.d #:: suffix.take(index - 1).map(_.d)

      def prefix(index: Int): Number =
        Number(prefixDigits(index).toList)
    }

    @tailrec
    def zippers(prefixMatches: Boolean, ps: (List[Position], Stream[Position]), qs: (List[Position], Stream[Position])): (PosZipper, PosZipper) =
      (ps._2, qs._2) match {
        case (PosZero #:: tp, PosZero #:: tq)        =>
          // End of p and q and all previous digits match.  Here we insert 0 and
          // 1 respectively to generate space between the two.  We know that
          // p < q because we filter equal ids out in the entry point but all
          // digits of p and q might be the same.
          (new PosZipper(ps._1, PosZero, ps._2),
           new PosZipper(qs._1, PosOne,  qs._2))

        case (hp #:: tp, hq #:: tq) if hp.d === hq.d =>
          zippers(prefixMatches && hp === hq, (hp :: ps._1, tp), (hq :: qs._1, tq))

        case _                                       =>
          // When the digits differ, if all previous positions are identical
          // and yet we know (p < q) then the remaining suffix of p must be less
          // than q.  Here we don't need a phantom 1 to find space between the
          // two. Otherwise the previous positions differ then the prefix of
          // p < q but the suffix may not be less so we insert 0 and 1.
          (new PosZipper(ps._1, PosZero,                           ps._2),
           new PosZipper(qs._1, prefixMatches ? PosZero | PosOne,  qs._2))
      }

    val (pz, qz) = zippers(prefixMatches = true, (Nil, posStream(p)), (Nil, posStream(q)))

    // Number of positions between p and q of length `index`.
    def interval(index: Int): BigInt =
      (qz.prefix(index) - pz.prefix(index) - Number.One).toBase10

    // Shortest index that contains enough space to hold `n` positions between
    // p and q.
    @tailrec
    def shortestPrefix(index: Int): Int =
      if (interval(index) < n) shortestPrefix(index + 1) else index

    // Gets the list of Digit that make up this Number, padded on the left with
    // zeros if necessary to equal the given width.
    def paddedDigits(num: Number, width: Int): List[Digit] = {
      def go(res: List[Digit], w: Int): List[Digit] =
        if (w >= width) res else go(Digit.Zero :: res, w + 1)

      go(num.toDigits, num.width)
    }

    def id(num: Number, width: Int, site: SiteId, timestamp: Timestamp): LineId = {
      @tailrec
      def go(rem: List[(Digit, Position)], matching: Boolean, res: List[Position]): LineId =
        rem match {
          case Nil                      =>
            val posList = res.reverse
            LineId.middle(posList.head, posList.tail)

          case (digit, pos) :: tail =>
            if (matching && digit === pos.d) go(tail, matching = true, pos :: res)
            else go(tail, matching = false, Position(digit, site, timestamp) :: res)
        }

      // Choose the pos zipper to match. Find the first position where p, num,
      // and q differ.  If num agrees with q, choose q but otherwise p.
      val digits  = paddedDigits(num, width)
      val pdigits = pz.prefixDigits(width)
      val qdigits = qz.prefixDigits(width)
      val z       = digits.zip(pdigits.zip(qdigits)).dropWhile {
        case (d, (p0, q0)) => (d === p0) && (d === q0)
      }.headOption.fold(pz) {
        case (d, (p0, q0)) => (d === q0) ? qz | pz
      }

      go(digits.tail.zip(z.suffix), matching = true, z.revPrefix)
    }

    val width    = shortestPrefix(1)
    val evenSize = interval(width) / n
    val step     = boundary.filter(_ < evenSize).map(BigInt(_)) | evenSize
    val stepNum  = Number.fromBase10(step)
    val start    = pz.prefix(width).toBase10
    val numList  = (0 until n).toList.map { i => Number.fromBase10(start + (step * i)) }

    for {
      sid  <- LogootDsl.site
      _    <- LogootDsl.tick
      time <- LogootDsl.timeNow
      nums <- numList.traverseU { num => LogootDsl.rand(stepNum).map(_ + num + Number.One) }
    } yield nums.map(id(_, width, sid, time))
  }

}
