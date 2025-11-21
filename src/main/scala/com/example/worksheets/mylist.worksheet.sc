enum MyList[A] {
    case Empty()
    case Pair(_head: A, _tail: MyList[A])

    def empty: Boolean = this match
        case Empty() => true
        case _ => false 

    def head: A = this match
        case Pair(_head, _tail) => _head
    
    def tail: MyList[A] = this match
        case Pair(_head, _tail) => _tail    
}

object MyList {
    
    def unfold[A, B](seed: A)(stop: A => Boolean, next: A => A, f: A => B): MyList[B] = 
        if stop(seed) then Empty()
        else Pair(f(seed), unfold(next(seed))(stop, next, f))

    def fill[A](n: Int)(elem: A) = unfold(n)(_ == 0, _ - 1, _ => elem)

    def iterate[A](start: A, len: Int)(f: A => A) = 
        unfold[(Int, A), A]((len, start))((n, _) => n == 0, (n, a) => (n - 1, f(a)), (_, a) => a)

    def map[A, B](xs: MyList[A])(f: A => B): MyList[B] = 
        unfold(xs)(_.tail.empty, _.tail, xs => f(xs.head))

}

MyList.fill(5)(1)
val test1 = MyList.iterate(1, 5)(n => n + 1)
MyList.iterate(1, 5)(n => n - 1)
MyList.map(test1)(_ * 2)