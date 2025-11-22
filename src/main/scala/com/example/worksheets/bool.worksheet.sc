trait Bool {
  def fold[A](t: A)(f: A): A
}

//Option(1).fold()

val True = new Bool {
  override def fold[A](t: A)(f: A): A = t
}

val False = new Bool {
  override def fold[A](t: A)(f: A): A = f
}

def and(l: Bool, r: Bool): Bool = new Bool {
  override def fold[A](t: A)(f: A): A = l.fold(r)(False).fold(t)(f)
}

and(True, True).fold("true")("false")
and(False, True).fold("true")("false")
and(True, False).fold("true")("false")
and(False, False).fold("true")("false")

def or(l: Bool, r: Bool): Bool = new Bool {
  override def fold[A](t: A)(f: A): A = r.fold(True)(l).fold(t)(f)
}

or(True, True).fold("true")("false")
or(False, True).fold("true")("false")
or(True, False).fold("true")("false")
or(False, False).fold("true")("false")

def not(b: Bool): Bool = new Bool {
  override def fold[A](t: A)(f: A): A = b.fold(f)(t)
}

not(False).fold("true")("false")
not(True).fold("true")("false")


