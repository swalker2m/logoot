package edu.gemini.logoot

import scalaz._

import LogootDsl._

/** Provides support for executing a logoot calculation. */
object Logoot {
  private object xform {
    type Result[A] = State[LogootState, A]

    val interp: LogootDsl ~> Result =
      new (LogootDsl ~> Result) {
        def apply[A](op: LogootDsl[A]): Result[A] =
          op match {
            case Site    => LogootState.site.st
            case TimeNow => LogootState.timestamp
            case Tick    => LogootState.timestamp.mods_(_.next)
            case Rand(n) =>
              for {
                r1     <- LogootState.rng.st
                (r2, i) = r1.nextNumber(n)
                _      <- LogootState.rng.assign(r2)
              } yield i
          }
      }
  }

  def point[A](a: A): Logoot[A] =
    Free.point[LogootDsl, A](a)

  def eval[A](prog: Logoot[A], siteId: SiteId, seed: Long): A =
    eval(prog, LogootState(siteId, seed))

  def eval[A](prog: Logoot[A], s: LogootState): A =
    run(prog, s)._2

  def run[A](prog: Logoot[A], siteId: SiteId, seed: Long): (LogootState, A) =
    run(prog, LogootState(siteId, seed))

  def run[A](prog: Logoot[A], s: LogootState): (LogootState, A) =
    prog.foldMap(xform.interp).run(s)
}
