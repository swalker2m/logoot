package edu.gemini

import scalaz._

package object logoot {
  type LogootDoc[A] = LineId ==>> A

  def emptyDoc[A]: LogootDoc[A] =
    ==>>.empty[LineId, A]
}
