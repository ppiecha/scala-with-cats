import cats.data._
import cats.instances._
import cats.syntax.all._

type Logger = Writer[Vector[String], Int]

def slowly[A](body: => A) =
  try body
  finally Thread.sleep(100)

def factorial(n: Int): Logger = slowly {
  if n == 0 then 1.writer(Vector(s"fact 0 1"))
  else factorial(n - 1).mapBoth{ (log, value) =>  
    val res = value * n 
    (log.appended(s"fact $n $res"), res)
  }
}

factorial(5)


import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
Await.result(
  Future.sequence(
    Vector(
      Future(factorial(5)),
      Future(factorial(5))
    )
  ),
  5.seconds
)

