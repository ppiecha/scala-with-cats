import cats._
import cats.syntax.all._

def compose[M[_]: Monad] = {
  type Composed[A] = M[Option[A]]
  new Monad[Composed] {
    def pure[A](a: A): M[Option[A]] = a.pure[Option].pure[M]
    def flatMap[A, B](fa: M[Option[A]])(f: A => Composed[B]): Composed[B] =
       fa.flatMap(_.fold(None.pure[M])(f))
  }
}
