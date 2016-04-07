package user

import cats._
import cats.data.{State, StateT, Xor}
import org.specs2.Specification
import Xor._
import org.atnos.eff.Tag.@@
import cats.syntax.foldable._
import cats.syntax.traverse._
import BiMap._

class TestSpec extends Specification { def is = s2"""

 test2 $test2
 test3 $test3

"""

  def test2 = {
    import org.atnos.eff._
    import StateEffect._
    import Eff._
    import DisjunctionEffect._
    import Test2._
    import Effects._

    val items = List(Item(Name("name1"), Id("id1")), Item(Name("name1"), Id("id1")))

    run(runDisjunction(
      runTaggedStateZero[SBMI |: XorDup |: NoEffect, XorDup |: NoEffect, Id, BM[Id], BM[Name]](
        execTaggedZero[S, SBMI |: XorDup |: NoEffect, Name, BM[Name], Unit](
          makeMaps(items)
        ): Eff[SBMI |: XorDup |: NoEffect, BM[Name]]
      )
    )).toEither must beLeft

  }

  def test3 = {
    import org.atnos.eff._
    import StateEffect._
    import Eff._
    import DisjunctionEffect._
    import Test3._
    import Monoidx._
    import Effects._

    def execZeroFirst[R <: Effects, St : Monoid, A](w: Eff[State[St, ?] |: R, A]): Eff[R, St] =
      execZero[State[St, ?] |: R, R, St, A](w)(Monoid[St], Member.ZeroMember[State[St, ?], R])

    val items = List(Item(Name("name1"), Id("id1")), Item(Name("name2"), Id("id2")))

    run(runDisjunctionEither(
        execZeroFirst(
          makeMaps(items)
        )
      )
    ) must beRight

  }
}

object Monoidx {
  implicit def PairMonoid[A : Monoid, B : Monoid]: Monoid[(A, B)] = new Monoid[(A, B)] {
    def empty = (Monoid[A].empty, Monoid[B].empty)
    def combine(m1: (A, B), m2: (A, B)) =
      (Monoid[A].combine(m1._1, m2._1), Monoid[B].combine(m1._2, m2._2))
  }
}

object Test2 {

  import org.atnos.eff._
  import Eff._
  import Effects._
  import StateCreation._
  import DisjunctionCreation._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import Member.<=
  import cats.std.list._

  type BM[K] = BiMap[K, Item]
  type SBMN[A] = State[BM[Name], A] @@ Name
  type SBMI[A] = State[BM[Id], A] @@ Id
  type XorDup[A] = DuplicateError Xor A

  type S = SBMN |: SBMI |: XorDup |: NoEffect

  def addItem[R](item: Item)(implicit s1: ({type l[X] = State[BM[Name], X] @@ Name})#l <= R,
                                      s2: ({type l[X] = State[BM[Id], X] @@ Id})#l <= R,
                                      e:  XorDup <= R): Eff[R, Unit] =
    for {
      nameMap  <- getTagged[R, Name, BM[Name]]
      idMap <- getTagged[R, Id, BM[Id]]
      _        <- updateMap(nameMap, item.name, item)
      _        <- updateMap(idMap, item.id, item)
    } yield ()

  def updateMap[R, K](bimap: BiMap[K, Item], key: K, item: Item)(
    implicit s: ({type l[X] = State[BM[K], X] @@ K})#l <= R,
             e: XorDup <= R): Eff[R, Unit] =
    bimap.getValue(key) match {
      case Some(_) => left(DuplicateError("boo"))
      case None    => modifyTagged[R, K, BM[K]](_ => bimap.add(key <-> item))
    }


  val as: List[Item] = List(Item(Name("name1"), Id("id1")))

  def makeMaps(as: List[Item]): Eff[S, Unit] = {
    import StateImplicits._
    as.traverse[Eff[S, ?], Unit](addItem[S]).as(())
  }
}

object Test3 {

  import org.atnos.eff._
  import Eff._
  import Effects._
  import StateCreation._
  import DisjunctionCreation._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.std.list._
  import Member.<=

  type BM[K] = BiMap[K, Item]
  type BMS = (BM[Name], BM[Id])
  type SBM[A] = State[BMS, A]
  type XorDup[A] = DuplicateError Xor A

  type S = SBM |: XorDup |: NoEffect

  def addItem[R](item: Item)(implicit s: SBM <= R, e:  XorDup <= R): Eff[R, Unit] =
    for {
      maps             <- get[R, BMS]
      (nameMap, idMap) =  maps
      updatedNames     <- updateMap[R, Name] (nameMap, item.name, item)
      updatedIds       <- updateMap[R, Id](idMap, item.id, item)
      _                <- put((updatedNames, updatedIds))
    } yield ()

  def updateMap[R, K](bimap: BM[K], key: K, item: Item)(implicit e: XorDup <= R): Eff[R, BM[K]] =
    bimap.getValue(key) match {
      case Some(_) => left(DuplicateError("boo"))
      case None    => right(bimap.add(key <-> item))
    }


  def makeMaps(as: List[Item]): Eff[S, Unit] = {
    import StateImplicits._
    as.traverseU(addItem[S]).as(())
  }
}

object Test4 {

  import org.atnos.eff._
  import Eff._
  import Effects._
  import StateEffect._
  import DisjunctionEffect._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.std.list._
  import cats.std.option._

  type BM[K] = BiMap[K, Item]
  type BMS = (BM[Name], BM[Id])
  type SBM[A] = State[BMS, A]
  type XorDup[A] = DuplicateError Xor A

  type S = SBM |: XorDup |: NoEffect

  def addItem(item: Item): Eff[S, Unit] =
    for {
      maps             <- get[S, BMS]
      (nameMap, idMap) =  maps
      updatedNames     <- fromXor[S, DuplicateError, BM[Name]](updateMap(nameMap, item.name, item))
      updatedIds       <- fromXor[S, DuplicateError, BM[Id]]  (updateMap(idMap, item.id, item))
      _                <- put[S, BMS]((updatedNames, updatedIds))
    } yield ()

  def updateMap[K](bimap: BM[K], key: K, item: Item): DuplicateError Xor BM[K] =
    Xor.fromOption(bimap.getValue(key).as(DuplicateError(s"$key is already present")), bimap.add(key <-> item)).swap

  // traverseU with no type annotations can be used here
  def makeMaps(as: List[Item]): Eff[S, Unit] =
    as.traverseU(addItem).as(())
}
case class Name(value: String) extends AnyVal
case class Id(value: String) extends AnyVal
case class Item(name: Name, id: Id)

case class DuplicateError(message: String)


object Test {
  import cats.std.list._

  type S = (BiMap[Name, Item], BiMap[Id, Item])

  implicit def m: Monad[StateT[DuplicateError Xor ?, S, ?]] =
    StateT.stateTMonadState[DuplicateError Xor ?, S]

  def update[K](map: BiMap[K, Item], key: Item => K, item: Item): DuplicateError Xor BiMap[K, Item] =
    map.getValue(key(item)) match {
      case Some(_) => Xor.left(DuplicateError("boo"))
      case None    => Xor.right(map.add(key(item) <-> item))
    }

  def addItem(item: Item): StateT[DuplicateError Xor ?, S, Unit] =
    StateT[DuplicateError Xor ?, S, Unit] { case (names, ids) =>
      for {
        m1 <- update(names, _.name, item)
        m2 <- update(ids, _.id, item)
      } yield ((m1, m2), ())
    }

  def makeMap(as: List[Item]): DuplicateError Xor (BiMap[Name, Item], BiMap[Id, Item]) =
    as.traverse[StateT[DuplicateError Xor ?, S, ?], Unit](addItem).runS((BiMap.empty, BiMap.empty))

}
case class BiMap[K, V](keys: Map[K, V], values: Map[V, K]) {

  def add(entry: BiMapEntry[K, V]): BiMap[K, V] =
    BiMap(keys + (entry.key -> entry.value), values + (entry.value -> entry.key))

  def getValue(k: K): Option[V] =
    keys.get(k)

  def getKey(v: V): Option[K] =
    values.get(v)
}

import cats.std.list._

object BiMap {

  def apply[K, V](entry: BiMapEntry[K, V]): BiMap[K, V] =
    BiMap.empty[K, V].add(entry)

  def empty[K, V]: BiMap[K, V] =
    BiMap(Map(), Map())

  implicit class createBiMapEntry[K](k: K) {
    def <->[V](v: V): BiMapEntry[K, V] =
      BiMapEntry[K,  V](k, v)
  }

  def fromList[K, V](list: List[BiMapEntry[K, V]]) =
    list.foldMap(e => BiMap.apply[K, V](e))

  implicit def MonoidBiMap[K, V]: Monoid[BiMap[K, V]] = new Monoid[BiMap[K, V]] {
    def empty = BiMap.empty[K, V]

    def combine(m1: BiMap[K, V], m2: BiMap[K, V]): BiMap[K, V] =
      BiMap(m1.keys ++ m2.keys, m1.values ++ m2.values)
  }
}

case class BiMapEntry[K, V](key: K, value: V)

