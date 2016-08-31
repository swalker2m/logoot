package edu.gemini.logoot

import scala.language.reflectiveCalls
import scalaz._, Scalaz._

import edu.gemini.logoot.LogootMessage.{Patch, Redo => MRedo, Undo => MUndo}

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

    def cemetery: LogootState[T] @> Cemetery =
      LogootState.cemetery[T]

    def patch(pid: PatchId): LogootState[T] @?> (Patch[T], Degree) =
      LogootState.patch[T](pid)

    val unit: Result[T, Unit] =
      State.state(())

    def point[A](a: => A): Result[T, A] =
      State.state(a)

    def idAt(index: Int): Result[T, LineId] =
      if (index < 0) State.state(LineId.Beginning)
      else doc.st.map(_.elemAt(index).map(_._1) | LineId.End)

    def executeOp(op: LogootOp[T]): Result[T, Unit] =
      op match {
        case LogootOp.Insert(lid, t) =>
          for {
            dg <- cemetery.map(_.get(lid).incr)
            _  <- if (dg === Degree.One) doc %== (_ + (lid -> t))
                  else cemetery %== (c => c.set(lid, dg))
          } yield ()

        case LogootOp.Delete(lid, _) =>
          for {
            c0 <- cemetery
            d0 <- doc
            (d1, dg) = if (d0.member(lid)) (d0 - lid, Degree.Zero) else (d0, c0.get(lid).decr)
            _  <- doc := d1
            _  <- cemetery %== (_.set(lid, dg))
          } yield ()
      }

    def execute(ops: List[LogootOp[T]]): Result[T, Unit] =
      ops.traverseU(executeOp).map(_ => ()) // no as(()) ?

    val emptyMessage: Result[T, Option[LogootMessage[T]]] =
      point(Option.empty[LogootMessage[T]])

    def initiateMessage(pid: PatchId, pred: Degree => Boolean)(op: (Patch[T], Degree) => Result[T, LogootMessage[T]]): Result[T, Option[LogootMessage[T]]] =
      for {
        t <- patch(pid)
        r <- t.filter { case (_, d) => pred(d) }.fold(emptyMessage) { case (p, d) =>
               op(p, d).map(r => Some(r))
             }
      } yield r

    def receiveMessage(pid: PatchId, thatDeg: Degree, pred: Degree => Boolean)(ops: Patch[T] => List[LogootOp[T]]): Result[T, Unit] =
      for {
        t <- patch(pid) %= { case (p, d) => (p, d + thatDeg) }
        _ <- t.filter { case (_, d) => pred(d) }.fold(unit) { case (p, _) =>
               execute(ops(p))
             }
      } yield ()

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
                _ <- history %== (_ + (p.pid -> (p, Degree.One)))
                _ <- session := List.empty
              } yield p

              // Lookup the corresponding patch.  If it exists and has a value
              // greater than 0, undo the operations, assign a zero degree, and
              // create an Undo message with the degree.  Otherwise, do nothing.
            case Undo(pid) =>
              initiateMessage(pid, _ > Degree.Zero) { (p, d) =>
                for {
                  _ <- execute(p.inverse)
                  _ <- patch(pid) := (p, Degree.Zero)
                } yield MUndo(pid, -d)
              }

            case Redo(pid) =>
              initiateMessage(pid, _ <= Degree.Zero) { (p, d) =>
                for {
                  _ <- execute(p.ops)
                  _ <- patch(pid) := (p, Degree.One)
                } yield MRedo(pid, -d + Degree.One)
              }

            case Receive(p@Patch(pid, ops)) =>
              for {
                _ <- history %== (_ + (p.pid -> (p, Degree.One)))
                _ <- execute(p.ops)
              } yield ()

            case Receive(MUndo(pid, thatDeg)) =>
              receiveMessage(pid, thatDeg, _ === Degree.Zero)(_.inverse)

            case Receive(MRedo(pid, thatDeg)) =>
              receiveMessage(pid, thatDeg, _ === Degree.One)(_.ops)
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