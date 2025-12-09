package com.example

import cats.data.State
import cats.syntax.all.*

object AnsiCodes {
  val csiString: String = "\u001b["
  def csi(arg: String, terminator: String): String =
    s"${csiString}${arg}${terminator}"
// SGR stands for Select Graphic Rendition.
// All the codes that change formatting are SGR codes.
  def sgr(arg: String): String =
    csi(arg, "m")
  val reset: String   = sgr("0")
  val boldOn: String  = sgr("1")
  val boldOff: String = sgr("22")
  val red: String     = sgr("31")
  val blue: String    = sgr("34")
}

final case class Terminal(bold: Int, color: List[String]) {
  def boldOn: Terminal               = this.copy(bold = bold + 1)
  def boldOff: Terminal              = this.copy(bold = bold - 1)
  def pushColor(c: String): Terminal = this.copy(color = c :: color)
// Only call this when we know there is at least one color on the
// stack
  def popColor: Terminal        = this.copy(color = color.tail)
  def peekColor: Option[String] = this.color.headOption
}

object Terminal {
  val empty: Terminal = Terminal(0, List.empty)
}

type Program[A] = State[Terminal, A]

object Program {

  def print(output: String): Program[Unit] =
    State[Terminal, Unit](terminal => (terminal, Console.print(output)))
  
  def bold[A](program: Program[A]): Program[A] =
    for {
      _ <- State.modify[Terminal] { terminal =>
        if terminal.bold == 0 then Console.print(AnsiCodes.boldOn)
        terminal.boldOn
      }
      a <- program
      _ <- State.modify[Terminal] { terminal =>
        val newTerminal = terminal.boldOff
        if terminal.bold == 0 then Console.print(AnsiCodes.boldOff)
        newTerminal
      }
    } yield a
// Helper to construct methods that deal with color
  def withColor[A](code: String)(program: Program[A]): Program[A] =
    for {
      _ <- State.modify[Terminal] { terminal =>
        Console.print(code)
        terminal.pushColor(code)
      }
      a <- program
      _ <- State.modify[Terminal] { terminal =>
        val newTerminal = terminal.popColor
        newTerminal.peekColor match {
          case None    => Console.print(AnsiCodes.reset)
          case Some(c) => Console.print(c)
        }
        newTerminal
      }
    } yield a
  
  def red[A](program: Program[A]): Program[A] =
    withColor(AnsiCodes.red)(program)
  
  def blue[A](program: Program[A]): Program[A] =
    withColor(AnsiCodes.blue)(program)
  
  def run[A](program: Program[A]): A = program.runA(Terminal.empty).value
}

@main def goState(): Unit = {
  val program =
    Program.blue(
      Program.print("This is blue ") >>
        Program.red(Program.print("and this is red ")) >>
        Program.bold(Program.print("and this is blue and bold "))
    ) >>
      Program.print("and this is back to normal.\n")
  Program.run(program)
}
