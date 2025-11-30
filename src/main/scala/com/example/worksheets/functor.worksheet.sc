import cats.Functor
import cats.syntax.all._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A)                        extends Tree[A]

object Tree {
  given Functor[Tree] with {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value)         => Leaf(f(value))
  }
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A]                        = Leaf(value)
}

import Tree._
val f = Functor[Tree]
val t = branch(branch(leaf(1), leaf(2)), leaf(3))
f.map(t)(_ + 1)
t.map(_ + 1)

trait Display[A] { self =>
  def display(value: A): String
  def contramap[B](f: B => A): Display[B] = new Display[B] {
    override def display(value: B): String = self.display(f(value))
  }
}

def display[A](value: A)(using p: Display[A]): String =
  p.display(value)

given stringDisplay: Display[String] with {
  def display(value: String): String =
    s"'${value}'"
}

given booleanDisplay: Display[Boolean] with {
  def display(value: Boolean): String =
    if value then "yes" else "no"
}

final case class Box[A](value: A)

given boxDisplay[A](using d: Display[A]): Display[Box[A]] = d.contramap(_.value)

display(Box("hello world"))
// res4: String = "'hello world'"
display(Box(true))
// res5: String = "yes"
// display(Box(123))

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))
      override def decode(value: String): B = dec(self.decode(value))
    }
}

def encode[A](value: A)(using c: Codec[A]): String =
  c.encode(value)
def decode[A](value: String)(using c: Codec[A]): A =
  c.decode(value)

given stringCodec: Codec[String] with {
  def encode(value: String): String = value
  def decode(value: String): String = value
}

given intCodec: Codec[Int] =
  stringCodec.imap(_.toInt, _.toString)
given booleanCodec: Codec[Boolean] =
  stringCodec.imap(_.toBoolean, _.toString)
given doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

encode(true)
encode(1)
encode(1.0)

given boxCodec[A](using c: Codec[A]): Codec[Box[A]] = stringCodec.imap(s => Box(c.decode(s)), _.value.toString)

encode(Box("1"))

encode(Box(123.4))
// res13: String = "123.4"
decode[Box[Double]]("123.4")
// res14: Box[Double] = Box(value = 123.4)





