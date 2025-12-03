import cats.Eval

def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match
    case head :: next => f(head, foldRight(next, z)(f))
    case Nil => z

def foldRightStackSafe[A, B](list: List[A], z: B)(f: (A, B) => B): Eval[B] = list match
    case head :: next => Eval.defer(foldRightStackSafe(next, z)(f)).map(b => f(head, b))
    case Nil => Eval.now(z)

foldRightStackSafe(List(1, 2, 3), 0)(_ + _).value


