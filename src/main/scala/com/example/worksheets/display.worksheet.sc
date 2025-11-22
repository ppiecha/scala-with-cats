trait Display[A] {
  def display(a: A): String
}

object Display {
  given Display[String] with
    def display(a: String): String = a
  given Display[Int] with
    def display(a: Int): String = a.toString()
  def display[A](a: A)(using d: Display[A]): String = d.display(a)
  def print[A](a: A)(using d: Display[A]): Unit     = println(display(a))
}

Display.display(1)
Display.print("1")

final case class Cat(name: String, age: Int, color: String)

object Cat {
  given Display[Cat] with
    def display(c: Cat): String = s"${c.name} is a ${c.age} year-old ${c.color} cat"
}

val cat = Cat("Lotka", 5, "grey")
Display.display(cat)

object DisplaySyntax {
  extension [A](a: A)(using d: Display[A])
    def display: String = d.display(a)
    def print: Unit     = println(display)
}

import DisplaySyntax._
cat.display
cat.print
