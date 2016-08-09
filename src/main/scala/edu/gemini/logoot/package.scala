package edu.gemini

import scalaz._

package object logoot {
  type Logoot[A] = Free[LogootDsl, A]

  type LogootDoc[A] = LineId ==>> A
}
