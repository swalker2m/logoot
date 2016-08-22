package edu.gemini.logoot

import edu.gemini.logoot.LogootOp.{Delete, Insert}

/** Logoot algorithm operations. Operations are produced by local edits so that
  * they can be easily shipped to and applied at peers. */
sealed trait LogootOp[A] {
  def apply(doc: LogootDoc[A]): LogootDoc[A] =
    this match {
      case Insert(id, a) => doc + (id -> a)
      case Delete(id, _) => doc - id
    }

  def inverse: LogootOp[A] =
    this match {
      case Insert(id, a) => Delete(id, a)
      case Delete(id, a) => Insert(id, a)
    }
}

object LogootOp {
  /** Inserts the content associated with the given id into the document. */
  case class Insert[A](id: LineId, content: A) extends LogootOp[A]

  /** Deletes the content associated with the given id from the document. */
  case class Delete[A](id: LineId, content: A) extends LogootOp[A]

  def insert[A](id: LineId, content: A): LogootOp[A] =
    Insert(id, content)

  def delete[A](id: LineId, content: A): LogootOp[A] =
    Delete(id, content)
}
