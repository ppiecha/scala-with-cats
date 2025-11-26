package com.example

import cats.syntax.all._

class CatsShowTest extends org.scalatest.funsuite.AnyFunSuite {
  test("cat.show") {
    val cat = Cat("Lotka", 5, "grey")
    assert(cat.show == "Lotka is a 5 year-old grey cat")
  }
}
