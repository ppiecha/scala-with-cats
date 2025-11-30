import cats.Monoid

Monoid[String].combine("Hi", "There")

def add[A](items: List[A])(using m: Monoid[A]): A = items.foldRight(m.empty)((a, b) => m.combine(a, b)) 
def add2[A](items: List[A])(using m: Monoid[A]): A = m.combineAll(items)

add(List(1, 2, 3))
add2(List(Option(1), Option(2), None))

case class Order(totalCost: Double, quantity: Double)

given Monoid[Order] = Monoid.instance(Order(0, 0), (o1, o2) => Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity))

val o1 = Order(1.2, 4.3)
val o2 = Order(2.8, 0.7)
add(List(o1, o2))


