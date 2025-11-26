import cats._
import cats.syntax.all._

val showInt = Show[Int]()
showInt.show(1)
1.show
