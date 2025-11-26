package com.example

import cats.syntax.all._

class CatsShowTest extends org.scalatest.funsuite.AnyFunSuite {
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  test("cat.show") {
    val cat = Cat("Lotka", 5, "grey")
    assert(cat.show == "Lotka is a 5 year-old grey cat")
  }
  test("cat.eq.simple") {
    assert(cat1 =!= cat2)
  }
  test("cat.eq.option") {
    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]
    assert(optionCat1 =!= optionCat2)
  }
}
