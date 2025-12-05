import cats.Monad
type Id[A] = A

given Monad[Id] with {
  override def pure[A](x: A): Id[A]                           = x
  override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  override def map[A, B](fa: Id[A])(f: A => B): Id[B]         = f(fa)
  override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match
    case Left(a)  => tailRecM(a)(f)
    case Right(b) => b

}

Monad[Id].pure(1)

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
  Branch(left, right)

def leaf[A](value: A): Tree[A] =
  Leaf(value)

given Monad[Tree] with {
  override def pure[A](x: A): Tree[A] = leaf(x)
  override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match
    case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
    case Leaf(value)         => f(value)

  override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = flatMap(f(a)) { 
    case Left(a) => tailRecM(a)(f)
    case Right(b) => leaf(b)
  }

}
