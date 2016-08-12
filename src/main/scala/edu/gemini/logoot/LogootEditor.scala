package edu.gemini.logoot

import edu.gemini.logoot.LogootOp.patch

import scalaz._
import Scalaz._

/** Basic editing support for working with a shared list of A.
  *
  * @tparam A type of the elements in the shared list
  */
trait LogootEditor[A] {

  type Result[B] = State[LogootState[A], B]

  val unit: Result[Unit] =
    State.state(())

  def point[B](b: B): Result[B] =
    State.state(b)

  /** Extracts the shared list. */
  def read: Result[List[A]] =
    LogootState.doc[A].st.map(_.values)

  /** Extracts the shared list in its editing context as a Logoot document. */
  def doc: Result[LogootDoc[A]] =
    get.map(_.doc)

  /** Initializes the editing context and returns the resulting Logoot
    * document.
    */
  def init(as: List[A]): Result[LogootDoc[A]] =
    for {
      _ <- insert(LineId.Beginning, LineId.End, as)
      d <- doc
    } yield d

  val site: Result[SiteId] =
    LogootState.idState[A].st.map(_.site)

  val timestamp: Result[Int] =
    LogootState.idState[A].st.map(_.timestamp.time)

  /** Finds the Logoot id corresponding to the given index.  Any negative value
    * is mapped to the `Beginning` id and any value greater than or equal to the
    * size of the shared list is mapped to the `End` id.
    *
    * @param index list index for which a Logoot id is sought
    *
    * @return corresponding Logoot index for the given list element
    */
  def idAt(index: Int): Result[LineId] =
    if (index < 0) point(LineId.Beginning)
    else LogootState.doc[A].st.map(_.elemAt(index).map(_._1) | LineId.End)

  /** Inserts the given content into the document between Logoot ids `p` and
    * `q`.  Returns an operation that can be applied by any remote peers to
    * match this local update.
    *
    * @param p id after which the content will be inserted
    * @param q id before which the content will be inserted
    * @param as content to insert
    *
    * @return an operation that can be applied in remote peers to track this
    *         local insertion
    */
  def insert(p: LineId, q: LineId, as: List[A]): Result[LogootOp[A]] =
    for {
      d0        <- doc
      is0       <- LogootState.idState[A].st
      (is1, ids) = GenerateLineId(p, q, as.size, None).run(is0)
      _         <- LogootState.idState[A] := is1
      entries    = ids.zip(as)
      _         <- LogootState.doc[A] := (d0/:entries) { _ + _ }
    } yield patch(entries.map { case (id, a) => LogootOp.insert(id, a) })

  /** Inserts the given content after `index`.
    *
    * @param index location in the shared list after which `as` should be
    *              inserted; use -1 to insert at the beginning of the list
    * @param as content to insert
    *
    * @return an operation that can be applied in remote peers to track this
    *         local insertion
    */
  def insert(index: Int, as: List[A]): Result[LogootOp[A]] =
    for {
      d0 <- doc
      p  <- idAt(index)
      q  <- idAt(index + 1)
      op <- insert(p, q, as)
    } yield op

  /** Deletes lines at id `p`, id `q` and everything inbetween.
    *
    * @param p first id whose associated content should be deleted
    * @param q last id whose associated content should be deleted
    *
    * @return an operation that can be applied in remote peers to track this
    *         local deletion
    */
  def delete(p: LineId, q: LineId): Result[LogootOp[A]] =
    for {
      d0      <- doc
      (_,  d1) = d0.split(p)
      (d2, _ ) = d1.split(q)
      ids      = p :: q :: d2.keys
      _       <- LogootState.doc[A] := (d0/:ids) { _ - _ }
    } yield patch(ids.map(LogootOp.delete[A]))

  /** Starting at `index`, deletes the next `count` items in the shared list.
    *
    * @param index index of the first item in the list to delete
    * @param count number of items to delete
    *
    * @return an operation that can be applied in remote peers to track this
    *         local deletion
    */
  def delete(index: Int, count: Int): Result[LogootOp[A]] =
    for {
      d0  <- doc
      ids <- (index until index + count).toList.traverseU(i => idAt(i)).map { ids =>
                ids.filter {
                  case _: LineId.Middle => true
                  case _                => false
                }
              }
      _   <- LogootState.doc[A] := (d0/:ids) { _ - _ }
    } yield patch(ids.map(LogootOp.delete[A]))

  /** Applies the given operation, presumably generated in a remote peer,  to
    * the local document.
    *
    * @param op operation to perform
    */
  def applyOp(op: LogootOp[A]): Result[Unit] =
    LogootState.doc[A].mods_(op.apply)
}