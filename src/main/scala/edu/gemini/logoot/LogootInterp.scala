package edu.gemini.logoot

import scala.language.reflectiveCalls
import scalaz._, Scalaz._

import edu.gemini.logoot.LogootMessage.{Patch, Redo, Undo}

object LogootInterp {
  type Result[T, A] = State[LogootState[T], A]

  private def xform[T](module: LogootModule[T]): module.Command ~> ({ type l[a] = Result[T, a] })#l = {

    def idState: LogootState[T] @> LineIdState =
      LogootState.idState[T]

    def doc: LogootState[T] @> LogootDoc[T] =
      LogootState.doc[T]

    def session: LogootState[T] @> List[List[LogootOp[T]]] =
      LogootState.session[T]

    def history: LogootState[T] @> HistoryBuffer[T] =
      LogootState.history[T]

    def patch(pid: PatchId): LogootState[T] @?> (Patch[T], Degree) =
      LogootState.patch[T](pid)

    val unit: Result[T, Unit] =
      State.state(())

    def idAt(index: Int): Result[T, LineId] =
      if (index < 0) State.state(LineId.Beginning)
      else doc.st.map(_.elemAt(index).map(_._1) | LineId.End)

    def execute(ops: List[LogootOp[T]]): Result[T, Unit] =
        doc %== { d0 => (d0/:ops) { case (d,o) => o(d) } }

    import module._

    new (Command ~> ({ type l[a] = Result[T, a] })#l) {
      override def apply[A](cmd: Command[A]): Result[T, A] =
          cmd match {
            case Read =>
              doc.st.map(_.values)

            case Insert(index, elements) =>
              for {
                d0        <- doc.st
                p         <- idAt(index - 1)
                q         <- idAt(index)
                is0       <- idState.st
                (is1, ids) = GenerateLineId(p, q, elements.size, None).run(is0)
                _         <- idState := is1
                entries    = ids.zip(elements)
                ops        = entries.map { case (i, a) => LogootOp.insert(i, a) }
                _         <- session %== (s => ops :: s)
                _         <- execute(ops)
              } yield ()

            case Delete(index, count) =>
              for {
                d0     <- doc.st
                (l,tmp) = d0.toList.splitAt(index)
                (del,r) = tmp.splitAt(count)
                ops     = del.map { case (i,t) => LogootOp.delete(i, t) }
                _      <- doc := ==>>.fromList(l).union(==>>.fromList(r))
                _      <- session %== (s => ops :: s)
              } yield ()

            case FinishEdit =>
              for {
                s <- session.st
                p  = Patch(PatchId.random, s.reverse.flatten)
                _ <- patch(p.pid) := (p, Degree.One)
                _ <- session := List.empty
              } yield p

            case Receive(p@Patch(pid, ops)) =>
              for {
                _ <- patch(pid) := (p, Degree.One)
                _ <- execute(p.ops)
              } yield ()

            case Receive(Undo(pid)) =>
              for {
                t <- patch(pid) %= { case (p, deg) => (p, deg.decr) }
                _ <- t.filter(_._2 === Degree.Zero).fold(unit) { case (p, _) => execute(p.inverse) }
              } yield ()

            case Receive(Redo(pid)) =>
              for {
                t <- patch(pid) %= { case (p, d) => (p, d.incr) }
                _ <- t.filter(_._2 === Degree.One).fold(unit) { case (p, _) => execute(p.ops) }
              } yield ()
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