trait Set[A] {
  def contains(elt: A): Boolean
  def insert(elt: A): Set[A] = {
    val self = this
    new Set[A]{
      override def contains(a: A): Boolean = a == elt || self.contains(a)
    }
  }
  def union(that: Set[A]): Set[A] = {
    val self = this
    new Set[A] {
        override def contains(a: A): Boolean = self.contains(a) || that.contains(a)
      }
  }
}

final case class ListSet[A](elements: List[A]) extends Set[A] {
  override def contains(elt: A): Boolean   = elements.contains(elt)
  override def insert(elt: A): Set[A]      = if !contains(elt) then ListSet(elt :: elements) else this
  override def union(that: Set[A]): Set[A] = elements.foldRight(that)((elt, set) => set.insert(elt))
  override def toString(): String          = elements.toString()
}

ListSet(List(1, 2, 3))
ListSet(List(1, 2, 3)).insert(4)
ListSet(List(1, 2, 3)).insert(1)
ListSet(List(1, 2, 3)).union(ListSet(List(1, 4)))

final case class InfSet[A](isIn: A => Boolean) extends Set[A] {
  def contains(elt: A): Boolean = isIn(elt)
  // def insert(elt: A): Set[A] = InfSet(e => e == elt || isIn(e))
  // def union(that: Set[A]): Set[A] = InfSet(e => isIn(e) || that.contains(e))
}

val s1 = InfSet[Int](_ > 0)
s1.contains(0)
s1.contains(1)
val s2 = s1.insert(0)
s2.contains(0)
val s3 = s1.union(s2)
s3.contains(-1)
s3.contains(1)
s3.contains(0)

