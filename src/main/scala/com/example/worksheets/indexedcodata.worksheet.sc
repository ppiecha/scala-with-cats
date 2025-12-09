trait Newtons
trait NewtonMetres

final case class Torque[Unit](value: Double)

// Weird units
trait Metres
trait Feet
trait Pounds
trait PoundsFeet
// An instance exists if A * B = C
trait Multiply[A, B, C]
object Multiply {
  given Multiply[Metres, Newtons, NewtonMetres] = new Multiply {}
  given Multiply[Feet, Pounds, PoundsFeet]      = new Multiply {}
}

final case class Length[L](value: Double) {
  def *[F, T](that: Force[F])(using Multiply[L, F, T]): Torque[T] =
    Torque(this.value * that.value)
}
final case class Force[F](value: Double) {
  def *[L, T](that: Length[L])(using Multiply[F, L, T]): Torque[T] =
    Torque(this.value * that.value)
}

Length[Metres](3) * Force[Newtons](4)
// res11: Torque[NewtonMetres] = Torque(value = 12.0)
Length[Feet](3) * Force[Pounds](4)
// res12: Torque[PoundsFeet] = Torque(value = 12.0)
