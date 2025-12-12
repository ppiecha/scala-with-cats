import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats._
import cats.syntax.all._

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Int
}

class UptimeService[F[_]](client: UptimeClient[F])(using Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] = hostnames.traverse(client.getUptime).map(_.sum)
}
