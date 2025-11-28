trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
    def apply[A](using m: Monoid[A]) = m
}

given andMonoid: Monoid[Boolean] with {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
}

given orMonoid: Monoid[Boolean] with {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
}