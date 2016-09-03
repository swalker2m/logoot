package edu.gemini.logoot

import org.scalacheck.Prop.{forAll, _}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification


object CemeterySpec extends Specification with ScalaCheck with Arbitraries {
  "Cemetery" should {
    "default to zero if not defined" !
      forAll { (id: LineId) =>
        Cemetery.Empty.get(id) == Degree.Zero
      }

    "obtain corresponding map value if defined" !
      forAll { (c: Cemetery, x: Int) =>
        c.m.size match {
          case 0 => true
          case s =>
            c.m.elemAt((x % s).abs).forall { case (id, deg) =>
              c.get(id) == deg
            }
        }
      }

    "removes the value if set to zero" !
      forAll { (c: Cemetery, x: Int) =>
        c.m.size match {
          case 0 => true
          case s =>
            val id = c.m.keys((x % s).abs)
            !c.set(id, Degree.Zero).m.member(id)
        }
      }

    "sets the value if not zero" !
      forAll { (c: Cemetery, id: LineId, d: Degree) =>
        (d == Degree.Zero) || c.set(id, d).m.lookup(id).contains(d)
      }

    "modifies the degree according to a provided function" !
      forAll { (c: Cemetery, x: Int, id: LineId) =>
        c.m.size match {
          case 0 =>
            c.mod(id)(_.incr).get(id) == Degree.One

          case s =>
            val id = c.m.keys((x % s).abs)
            c.mod(id)(_.incr).get(id) == c.get(id).incr
        }
      }
  }

}
