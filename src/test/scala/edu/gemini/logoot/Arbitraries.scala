package edu.gemini.logoot

import org.scalacheck.Arbitrary._
import org.scalacheck._

import java.util.UUID

import scalaz.NonEmptyList

trait Arbitraries {
  implicit val arbDigit: Arbitrary[Digit] =
    Arbitrary {
      Gen.chooseNum(0, Digit.Base - 1).map(Digit.fromInt)
    }

  implicit val arbNumber: Arbitrary[Number] =
    Arbitrary {
      arbitrary[List[Digit]].map(Number(_))
    }

  implicit val arbTimestamp: Arbitrary[Timestamp] =
    Arbitrary {
      arbitrary[Int].map(Timestamp(_))
    }

  implicit val arbSiteId: Arbitrary[SiteId] =
    Arbitrary {
      Gen.uuid.map(u => SiteId(u))
    }

  implicit val arbPosition: Arbitrary[Position] =
    Arbitrary {
      for {
        d <- arbitrary[Digit]
        s <- arbitrary[SiteId]
        t <- arbitrary[Timestamp]
      } yield Position(d, s, t)
    }

  import LineId.{Beginning, Middle, End}

  implicit val arbMiddle: Arbitrary[Middle] =
    Arbitrary {
      for {
        d <- Gen.chooseNum(1, Digit.Base - 1)
        s <- arbitrary[SiteId]
        t <- arbitrary[Timestamp]

        head  = Position(Digit(d), s, t)
        tail <- arbitrary[List[Position]]
      } yield Middle(NonEmptyList[Position](head, tail: _*))
    }

  implicit val arbElementId: Arbitrary[LineId] =
    Arbitrary {
      Gen.frequency[LineId](
         1 -> Beginning,
        98 -> arbitrary[Middle],
         1 -> End)
    }

  implicit val arbLineIdState: Arbitrary[LineIdState] =
    Arbitrary {
      for {
        sid  <- arbitrary[SiteId]
        seed <- arbitrary[Long]
      } yield LineIdState(sid, seed)
    }
}
