import cats.Monad
type Id[A] = A

given Monad[Id] with {
  override def pure[A](x: A): Id[A]                           = x
  override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
  override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match
    case Left(a)  => tailRecM(a)(f)
    case Right(b) => b

}

Monad[Id].pure(1)

