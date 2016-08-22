package edu.gemini

import edu.gemini.logoot.LogootMessage.Patch

import scalaz._

package object logoot {
  type LogootDoc[A]     = LineId  ==>> A
  type HistoryBuffer[A] = PatchId ==>> (Patch[A], Degree)

  def emptyDoc[A]: LogootDoc[A] =
    ==>>.empty[LineId, A]

  def emptyBuffer[A]: HistoryBuffer[A] =
    ==>>.empty[PatchId, (Patch[A], Degree)]
}
