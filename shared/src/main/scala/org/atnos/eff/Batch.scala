package org.atnos.eff

/**
 * This trait provides a way to rewrite applicative effects
 * when there is an operation allowing the batching of some effects based on the Batchable typeclass
 */
trait Batch {

  def batch[R, T[_], A](eff: Eff[R, A])(implicit batchable: Batchable[T], m: T /= R): Eff[R, A] =
    eff match {
      case ImpureAp(unions, continuation, last) =>
        // extract only the effects of type M
        val collected = unions.extract

        // we zip each effect with its indice in the original ImpureAp effect list
        // this allows us to recreate a 'map' function for the rewritten ImpureAp
        // where the result of each effect after interpretation will be at the right place as a argument to the
        // 'map' function
        collected.effects zip collected.indices match {
          case v if v.isEmpty => eff

          case e +: rest =>
            // batch any effects which can be batched together
            // by using the Batched datastructure keeping track
            // of both unbatched and batch effects
            val result: Batched[T] = rest.foldLeft(Batched.single(e)) { case (batched, (effect, i)) =>
              batchable.batch(batched.batchedEffect, effect) match {
                case Some(b) => batched.fuse(b, i)
                case None => batched.append(effect, i)
              }
            }

            result.effects match {
              case v if v.isEmpty =>
                eff

              case (e1: T[_]) +: rest1 =>
                ImpureAp(
                  Unions(m.inject(e1), rest1.map(r => m.inject(r.asInstanceOf[T[Any]])) ++ collected.otherEffects),
                  // the map operation has to reorder the results based on what could be batched or not
                  continuation.contramap(ls => reorder(ls, result.keys ++ collected.otherIndices)),
                  last
                )
            }
        }

      case _ => eff
    }

  // reorder an input list based on the expected indices for that list
  private def reorder[T[_]](ls: Vector[Any], indices: Vector[Int])(implicit batchable: Batchable[T]): Vector[Any] =
    indices.zip(flatten(ls)).sortBy(_._1).map(_._2)

  // the result of batching
  private def flatten[T[_]](ls: Vector[Any])(implicit batchable: Batchable[T]): Vector[Any] =
    ls match {
      case xs :+ z =>
        xs ++ batchable.distribute(z.asInstanceOf[batchable.Z])

      case v if v.isEmpty =>
        Vector.empty
    }

}

object Batch extends Batch

trait Batchable[T[_]] {
  type Z
  type E
  def distribute(z: Z): List[E]
  def batch[X, Y](t1: T[X], t2: T[Y]): Option[T[Z]]
}

/**
 * The Batched classes are used to store unbatched and batched effects
 * depending on the result of the Batchable typeclass
 *
 * The assumption is that the order of the effects in 'effects'
 * correspond to the order of the keys in 'keys'
 *
 */
private sealed trait Batched[T[_]] {
  type X
  def effects: Vector[T[Any]]
  def keys: Vector[Int]
  def batchedEffect: T[X]

  def append[Y](ty: T[Y], key: Int): Batched[T]
  def fuse[Y](ty: T[Y], key: Int): Batched[T]
}

private object Batched {
  def single[T[_], X](txi: (T[X], Int)): Batched[T] =
    Single(txi._1, Vector(txi._2))

  type Aux[T[_], Y] = Batched[T] { type X = Y }
}

private case class Composed[T[_]](unbatched: Vector[Batched[T]], batched: Single[T, ?]) extends Batched[T] {
  override type X = Any

  def effects: Vector[T[Any]] = unbatched.flatMap(_.effects)
  def keys: Vector[Int] = unbatched.flatMap(_.keys) ++ batched.keys
  def batchedEffect: T[X] = batched.batchedEffect.asInstanceOf[T[X]]

  def append[Y](ty: T[Y], key: Int): Batched[T] =
    copy(unbatched = unbatched :+ Batched.single[T, Y]((ty, key)))

  def fuse[X](ty: T[X], key: Int): Batched[T] =
    copy(batched = Single[T, X](ty, batched.keys :+ key))
}

private case class Single[T[_], Y](tx: T[Y], keys: Vector[Int]) extends Batched[T] {
  override type X = Y

  def effects: Vector[T[Any]] = Vector(tx.asInstanceOf[T[Any]])
  def batchedEffect = tx

  def append[Y](ty: T[Y], key: Int): Batched[T] =
    Composed[T](Vector(Batched.single[T, X]((tx, key))), this)

  def fuse[Z](ty: T[Z], key: Int): Batched[T] =
    Single(ty, keys :+ key)
}
