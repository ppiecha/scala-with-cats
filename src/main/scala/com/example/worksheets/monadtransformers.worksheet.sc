import cats._
import cats.syntax.all._
import cats.data.OptionT
import cats.data.EitherT
import scala.concurrent.Future

def compose[M[_]: Monad] = {
  type Composed[A] = M[Option[A]]
  new Monad[Composed] {
    override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    def pure[A](a: A): M[Option[A]]                                                = a.pure[Option].pure[M]
    def flatMap[A, B](fa: M[Option[A]])(f: A => Composed[B]): Composed[B] =
      fa.flatMap(_.fold(None.pure[M])(f))
  }
}

type ListOption[A] = OptionT[List, A]

val l1: ListOption[Int] = OptionT(List(Some(1)))
val l2: ListOption[Int] = OptionT(List(Some(2)))
val l3: ListOption[Int] = OptionT(List(None))

val lo = for {
  a <- l1
  b <- l2
  c <- l3
} yield a + b + c

lo.value

// future -> either -> option

type FutureEither[A]       = EitherT[Future, String, A]
type FutureEitherOption[A] = OptionT[FutureEither, A]

import cats.instances.future._ // for Monad
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

val powerLevels = Map(
  "Jazz"      -> 6,
  "Bumblebee" -> 8,
  "Hot Rod"   -> 10
)

type Response[A] = EitherT[Future, String, A]

def getPowerLevel(autobot: String): Response[Int] = EitherT(
  Future(powerLevels.get(autobot).toRight(s"Autobot $autobot not found"))
)

Await.result(getPowerLevel("Jazz").value, 1.second)
Await.result(getPowerLevel("xxx").value, 1.second)

def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
  a <- getPowerLevel(ally1)
  b <- getPowerLevel(ally2)
} yield a + b > 15

Await.result(canSpecialMove("Jazz", "Hot Rod").value, 1.second)
Await.result(canSpecialMove("Jazz", "Bumblebee").value, 1.second)
Await.result(canSpecialMove("Blue", "Bumblebee").value, 1.second)

def tacticalReport(ally1: String, ally2: String): String = {
  val resp = canSpecialMove(ally1, ally2).value
  try {
    Await
      .result(resp, 1.second)
      .fold(
        identity,
        {
          case true  => f"$ally1 $ally2 are ready to roll out!"
          case false => f"$ally1 $ally2 need a recharge."
        }
      )
  } catch { _ => "Timeout" }
}

tacticalReport("Jazz", "Bumblebee")
tacticalReport("Bumblebee", "Hot Rod")
tacticalReport("Jazz", "Ironhide")

