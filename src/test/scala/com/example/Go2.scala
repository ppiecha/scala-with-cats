package com.example

type Program[A] = () => A

//val csiString   = "\u001b["
val redCode     = s"${csiString}31m"
val resetCode   = s"${csiString}0m"
val boldOnCode  = s"${csiString}1m"
val boldOffCode = s"${csiString}22m"

def run[A](program: Program[A]): A = program()

def print(output: String): Program[Unit] =
  () => Console.print(output)

def printRed2[A](output: Program[A]): Program[A] =
  () => {
    run(print(redCode))
    val result = run(output)
    run(print(resetCode))
    result
  }

def printBold2[A](output: Program[A]): Program[A] =
  () => {
    run(print(boldOnCode))
    val result = run(output)
    run(print(boldOffCode))
    result
  }

@main def go2(): Unit =
  run(() => {
    run(print("Normal text, "))
    run(printRed2(print("now red text, ")))
    run(printBold2(print("and now bold ")))
    run(printBold2(printRed2(print("and now bold and red.\n"))))
  })
