package edu.gemini.logoot

import edu.gemini.logoot.LogootMessage.{Patch, Undo => MUndo, Redo => MRedo}
import scalaz._

trait LogootModule[T] {
  sealed trait Command[A]

  case object Read                                       extends Command[List[T]]

  final case class Insert(index: Int, elements: List[T]) extends Command[Unit]
  final case class Delete(index: Int, count: Int)        extends Command[Unit]
  case object FinishEdit                                 extends Command[Patch[T]]

  final case class Undo(pid: PatchId)                    extends Command[Option[LogootMessage[T]]]
  final case class Redo(pid: PatchId)                    extends Command[Option[LogootMessage[T]]]

  final case class Receive(msg: LogootMessage[T])        extends Command[Unit]

  type Logoot[A] = Free[Command, A]

  def point[A](a: A): Logoot[A] =
    Free.point[Command, A](a)

  val read: Logoot[List[T]] =
    Free.liftF(Read)

  def insert(index: Int, elements: List[T]): Logoot[Unit] =
    Free.liftF(Insert(index, elements))

  def delete(index: Int, count: Int): Logoot[Unit] =
    Free.liftF(Delete(index, count))

  val finishEdit: Logoot[Patch[T]] =
    Free.liftF(FinishEdit)

  def undo(pid: PatchId): Logoot[Option[LogootMessage[T]]] =
    Free.liftF(Undo(pid))

  def redo(pid: PatchId): Logoot[Option[LogootMessage[T]]] =
    Free.liftF(Redo(pid))

  def receive(msg: LogootMessage[T]): Logoot[Unit] =
    Free.liftF(Receive(msg))
}