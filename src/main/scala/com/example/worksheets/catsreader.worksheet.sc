import cats.data._
import cats.syntax.all._
final case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

type DbReader[A] = Reader[Db, A]

def findUsername(userId: Int): DbReader[Option[String]] =
  Reader { db =>
    db.usernames.get(userId)
  }

def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader { db =>
    db.passwords.get(username).map(_ == password).getOrElse(false)
  }

def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  for {
    optUserName   <- findUsername(userId)
    passwordValid <- optUserName.map(userName => checkPassword(userName, password)).getOrElse(false.pure[DbReader])
  } yield passwordValid

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)
val passwords = Map(
  "dade"  -> "zerocool",
  "kate"  -> "acidburn",
  "margo" -> "secret"
)
val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)
// res7: Boolean = true
checkLogin(4, "davinci").run(db)
// res8: Boolean = false
