import cats._
import cats.syntax.all._
import cats.data.State

type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] =
  sym match {
    case "+" => calc(_ + _)
    case "*" => calc(_ * _)
    case d   => push(d.toInt)
  }

def push(n: Int): CalcState[Int] = State(stack => (n :: stack, n))

def calc(f: (Int, Int) => Int): CalcState[Int] = {
  State[List[Int], Int] {
    case b :: a :: tail =>
      val ans = f(a, b)
      (ans :: tail, ans)
    case _ => sys.error("Fail")
  }
}

evalOne("9").run(List()).value

val program = for {
  _   <- evalOne("1")
  _   <- evalOne("2")
  ans <- evalOne("+")
} yield ans

program.runA(Nil).value

def evalAll(seq: List[String]): CalcState[Int] = seq match
  case sym :: next => evalOne(sym).flatMap(_ => evalAll(next))
  case Nil         => State.inspect[List[Int], Int](_.head)

def evalAllFoldLeft(seq: List[String]): CalcState[Int] =
  seq.foldLeft(0.pure[CalcState])((st, sym) => st.flatMap(_ => evalOne(sym)))

evalAll(List("3", "2", "+")).run(List()).value
evalAllFoldLeft(List("3", "2", "+")).run(List()).value
evalAllFoldLeft(List("1", "2", "+", "3", "4", "+", "*")).run(List()).value

def evalInput(input: String): Int = evalAllFoldLeft(input.split(" ").toList).runA(Nil).value

evalInput("1 2 + 3 4 + *")