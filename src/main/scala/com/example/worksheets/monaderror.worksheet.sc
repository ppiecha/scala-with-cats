import cats._
import cats.syntax.all._
import scala.util.Try

def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = 
    age.pure[F].ensure(new IllegalArgumentException("Age must be greater than or equal to 18"))(_ >= 18)

validateAdult[Try](18)
validateAdult[Try](8)
type ExceptionOr[A] = Either[Throwable, A]
validateAdult[ExceptionOr](-1)


