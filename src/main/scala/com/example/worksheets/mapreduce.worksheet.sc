import cats._
import cats.syntax.all._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration._

def foldMap[A, B: Monoid](values: Vector[A])(f: A => B): B = values.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

foldMap(Vector(1, 2, 3))(identity)
import cats.instances.string._
foldMap(Vector(1, 2, 3))(_.toString + "! ")
foldMap("Hello world!".toVector)(_.toString.toUpperCase)

Runtime.getRuntime.availableProcessors

def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] =
  Future.sequence(
    values
      .grouped(Runtime.getRuntime.availableProcessors)
      .map(l => Future(foldMap(l)(func)))
  ).map(Monoid[B].combineAll)

def parallelFoldMapCats[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] =
    values
      .grouped(Runtime.getRuntime.availableProcessors)
      .toVector
      .traverse(l => Future(foldMap(l)(func)))
      .map(_.combineAll)


parallelFoldMap(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9))(identity)
val res = parallelFoldMap((1 to 1000000).toVector)(identity)
Await.result(res, 1.second)

def foldMapCats[A, B: Monoid](values: Vector[A])(f: A => B): B = values.foldMap(f)

val future = parallelFoldMapCats((1 to 1000).toVector)(_ * 1000)
Await.result(future, 1.second)
