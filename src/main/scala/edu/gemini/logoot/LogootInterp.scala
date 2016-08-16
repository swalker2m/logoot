package edu.gemini.logoot

import scalaz._, Scalaz._

import LogootOp.patch

object LogootInterp {
  type Result[T, A] = State[LogootState[T], A]

  private def xform[T](module: LogootModule[T]): module.Command ~> ({ type l[a] = Result[T, a] })#l = {

    def idState: LogootState[T] @> LineIdState =
      LogootState.idState[T]

    def doc: LogootState[T] @> LogootDoc[T] =
      LogootState.doc[T]

    def idAt(index: Int): Result[T, LineId] =
      if (index < 0) State.state(LineId.Beginning)
      else doc.st.map(_.elemAt(index).map(_._1) | LineId.End)

    import module._

    new (Command ~> ({ type l[a] = Result[T, a] })#l) {
      override def apply[A](cmd: Command[A]): Result[T, A] =
          cmd match {
            case Read =>
              doc.st.map(_.values)

            case Insert(index, elements) =>
              for {
                d0        <- doc.st
                p         <- idAt(index)
                q         <- idAt(index + 1)
                is0       <- idState.st
                (is1, ids) = GenerateLineId(p, q, elements.size, None).run(is0)
                _         <- idState := is1
                entries    = ids.zip(elements)
                _         <- doc := (d0 /: entries)(_ + _)
              } yield patch(entries.map { case (i, a) => LogootOp.insert(i, a) })

            case Delete(index, count) =>
              for {
                d0  <- doc.st
                ids <- (index until index + count).toList.traverseU(i => idAt(i)).map { ids =>
                  ids.filter {
                    case _: LineId.Middle => true
                    case _                => false
                  }
                }
                _   <- doc := (d0 /: ids)(_ - _)
              } yield patch(ids.map(LogootOp.delete[T]))

            case ApplyOp(op) =>
              doc.mods_(op.apply)
          }
      }
  }

  def run[T, A](module: LogootModule[T])(prog: module.Logoot[A], state: LogootState[T]): (LogootState[T], A) =
    prog.foldMap[({ type l[a] = Result[T, a] })#l](xform(module)).run(state)

  def eval[T, A](module: LogootModule[T])(prog: module.Logoot[A], state: LogootState[T]): A =
    run(module)(prog, state)._2

  def exec[T, A](module: LogootModule[T])(prog: module.Logoot[A], state: LogootState[T]): LogootState[T] =
    run(module)(prog, state)._1
}