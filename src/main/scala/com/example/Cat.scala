package com.example

import cats._
import cats.syntax.all._

final case class Cat(name: String, age: Int, color: String)

object Cat {
    given Show[Cat] = Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat")
}

@main
def testShow() = 
    val cat = Cat("Lotka", 5, "grey")
    println(cat.show)
