enum Tree[A] {

    case Leaf(a: A)
    case Node(left: Tree[A], right: Tree[A])

    def size: Int = this match
        case Leaf(_) => 1
        case Node(l, r) => l.size + r.size
    
    def contains(value: A): Boolean = this match {
        case Leaf(a) => a == value 
        case Node(left, right) => left.contains(value) || right.contains(value) 
    }

    def map[B](f: A => B): Tree[B] = this match
        case Leaf(a) => Leaf(f(a))
        case Node(left, right) => Node(left.map(f), right.map(f))

    def fold[B](leaf: A => B)(merge: (B, B) => B): B = this match
        case Leaf(a) => leaf(a)
        case Node(left, right) => merge(left.fold(leaf)(merge),  right.fold(leaf)(merge))   

    def sizeFold: Int = fold(_ => 1)((_, b) => b + 1)
    
    def containsFold(value: A): Boolean = fold(_ == value)((b1, b2) => b1 || b2)

    def mapFold[B](f: A => B): Tree[B] = fold[Tree[B]](a => Leaf(f(a)))((b1, b2) => Node(b1, b2))
    
}

import Tree._
val tree = Node(Leaf(1), Node(Leaf(2), Leaf(3)))
tree.size
tree.contains(2)
tree.contains(0)
tree.map(_ * 2)
tree.sizeFold
tree.containsFold(2)
tree.containsFold(0)
tree.mapFold(_ * 2)






