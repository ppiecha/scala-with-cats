final case class GCounter2(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int) = GCounter2(
    counters.updated(machine, counters.getOrElse(machine, 0) + amount)
  )
  def merge(that: GCounter2): GCounter2 = GCounter2(counters.map((k, v) => (k, v max that.counters.getOrElse(k, 0))))
  def total: Int                        = counters.values.sum
}

import cats.kernel.CommutativeMonoid
trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

given BoundedSemiLattice[Int] with {
  override def combine(a1: Int, a2: Int): Int = a1 max a2
  override def empty: Int                     = 0
}

given setInstance[A]: BoundedSemiLattice[Set[A]] with {
  override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2
  override def empty: Set[A]                           = Set()
}

import cats.syntax.all._
import cats.instances.map._

final case class GGCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)(using m: CommutativeMonoid[A]): GGCounter[A] = {
    val value = amount |+| counters.getOrElse(machine, m.empty)
    GGCounter(counters + (machine -> value))
  }
  def merge(that: GGCounter[A])(using m: BoundedSemiLattice[A]): GGCounter[A] = GGCounter(
    this.counters |+| that.counters
  )
  def total(using m: CommutativeMonoid[A]): A = counters.values.toList.combineAll
}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}
object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) =
    counter
}

given mapInstance[K, V]: GCounter[Map, K, V] with {
  def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
    val value = v |+| f.getOrElse(k, m.empty)
    f + (k -> value)
  }
  def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2
  def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.toList.combineAll
}
