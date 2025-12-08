import cats.Monoid
List(1, 2, 3).foldRight(Nil)((a, b) => a :: b)
List(1, 2, 3).foldLeft(Nil)((a, b) => b :: a)

def map[A, B](fa: List[A])(f: A => B): List[B] = fa.foldRight(Nil)((a, b) => f(a) :: b) 
def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.foldRight(Nil)((a, b) => f(a) ::: b)
def filter[A](fa: List[A])(p: A => Boolean): List[A] = fa.foldRight(Nil)((a, b) => if p(a) then a :: b else b)
def sum[A](fa: List[A])(using m: Monoid[A]): A = fa.foldRight(m.empty)(m.combine)

map(List(1, 2, 3))(_ * 2)
flatMap(List(1, 2, 3))(i => List(i, i * 10, i * 100))
filter(List(1, 2, 3))(_ % 2 == 1)
sum(List(1, 2, 3))



