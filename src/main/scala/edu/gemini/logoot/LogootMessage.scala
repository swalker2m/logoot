package edu.gemini.logoot

/** Logoot edit messages sent between peers. */
sealed trait LogootMessage[A]

object LogootMessage {

  final case class Patch[A](pid: PatchId, ops: List[LogootOp[A]]) extends LogootMessage[A] {
    def inverse: List[LogootOp[A]] =
      ops.map(_.inverse)
  }

  final case class Undo[A](pid: PatchId)                          extends LogootMessage[A]
  final case class Redo[A](pid: PatchId)                          extends LogootMessage[A]
}
