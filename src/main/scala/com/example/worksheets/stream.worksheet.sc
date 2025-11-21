trait Stream[A] {
  def head: A
  def tail: Stream[A]
  def map[B](f: A => B): Stream[B] = {
    val self = this
    new Stream {
        override def head: B = f(self.head)
        override def tail: Stream[B] = self.tail.map(f)

      }
  }
  def take(n: Int): List[A] = if n == 0 then Nil else head :: tail.take(n - 1)
  def filter(pred: A => Boolean): Stream[A] = {
    val self = this
    new Stream[A] {
      override def head: A = {
        def loop(stream: Stream[A]): A =
          if pred(stream.head) then stream.head else loop(stream.tail)
        loop(self)
      }
      override def tail: Stream[A] =
        def loop(stream: Stream[A]): Stream[A] = if pred(stream.head) then stream.tail.filter(pred)
        else loop(stream.tail)
        loop(self)
    }
  }
  def zip[B](that: Stream[B]): Stream[(A, B)] = {
    val self = this
    new Stream {
      override def head: (A, B)         = (self.head, that.head)
      override def tail: Stream[(A, B)] = self.tail.zip(that.tail)
    }
  }
  def scanLeft[B](z: B)(op: (B, A) => B): Stream[B] = {
    val self = this
    new Stream {
      override def head: B         = z
      override def tail: Stream[B] = self.tail.scanLeft(op(z, self.head))(op)

    }
  }

}

object Stream {
  def unfold[A, B](seed: A)(f: A => B, next: A => A): Stream[B] = new Stream {
    override def head: B         = f(seed)
    override def tail: Stream[B] = unfold(next(seed))(f, next)
  }
}

val ints = Stream.unfold(1)(identity, _ + 1)
ints.take(5)
val evens = ints.filter(_ % 2 == 0)
ints.zip(evens).take(5)
ints.scanLeft(0)((b, a) => a + b).take(5)
val naturals: Stream[Int] = new Stream[Int] {
    lazy val head: Int = 1
    lazy val tail: Stream[Int] = naturals.map(_ + 1)
}
naturals.take(7)

