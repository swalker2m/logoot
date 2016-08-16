package edu.gemini.logoot

import scalaz._

trait LogootModule[T] {
  sealed trait Command[A]

  case object Read                                 extends Command[List[T]]
  case class Insert(index: Int, elements: List[T]) extends Command[LogootOp[T]]
  case class Delete(index: Int, count: Int)        extends Command[LogootOp[T]]
  case class ApplyOp(op: LogootOp[T])              extends Command[Unit]

  type Logoot[A] = Free[Command, A]

  def point[A](a: A): Logoot[A] =
    Free.point[Command, A](a)

  val read: Logoot[List[T]] =
    Free.liftF(Read)

  def insert(index: Int, elements: List[T]): Logoot[LogootOp[T]] =
    Free.liftF(Insert(index, elements))

  def delete(index: Int, count: Int): Logoot[LogootOp[T]] =
    Free.liftF(Delete(index, count))

  def applyOp(op: LogootOp[T]): Logoot[Unit] =
    Free.liftF(ApplyOp(op))
}