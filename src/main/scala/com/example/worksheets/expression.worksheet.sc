type Continuation = Double => Call
enum Call {
  case Continue(value: Double, k: Continuation)
  case Loop(expr: Expression, k: Continuation)
  case Done(result: Double)
}
enum Expression {
  case Literal(d: Double)
  case Add(e1: Expression, e2: Expression)
  case Sub(e1: Expression, e2: Expression)
  case Mult(e1: Expression, e2: Expression)
  case Div(e1: Expression, e2: Expression)
  def +(that: Expression): Expression = Add(this, that)
  def -(that: Expression): Expression = Sub(this, that)
  def *(that: Expression): Expression = Mult(this, that)
  def /(that: Expression): Expression = Div(this, that)

  def eval: Double = {
    def loop(e: Expression, k: Continuation): Call = e match {
      case Literal(d)        => Call.Continue(d, k)
      case Add(left, right)  => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l + r, k)))
      case Sub(left, right)  => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l - r, k)))
      case Mult(left, right) => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l * r, k)))
      case Div(left, right)  => Call.Loop(left, l => Call.Loop(right, r => Call.Continue(l / r, k)))
    }

    def trampoline(next: Call): Double = next match {
      case Call.Continue(d, k) => trampoline(k(d))
      case Call.Loop(exp, k)   => trampoline(loop(exp, k))
      case Call.Done(d)        => d
    }
    trampoline(loop(this, d => Call.Done(d)))
  }

}

import Expression._
val e = Literal(2) + (Literal(2) * Literal(2))
e.eval


