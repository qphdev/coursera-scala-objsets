object IntSet {
  abstract class IntSet {
    def incl(x: Int): IntSet

    def contains(x: Int): Boolean

    def union(other: IntSet): IntSet
  }

  object Empty extends IntSet {
    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    override def contains(x: Int): Boolean = false

    override def union(other: IntSet): IntSet = other

    override def toString: String = "•"
  }

  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def contains(x: Int): Boolean = {
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true
    }

    override def incl(x: Int): IntSet = {
      if (x < elem) new NonEmpty(elem, left.incl(x), right)
      else if (x > elem) new NonEmpty(elem, left, right.incl(x))
      else this
    }

    override def union(other: IntSet): IntSet = {
      ((left union right) union other) incl elem
    }

    override def toString: String = s"<$left $elem $right>"
  }
}

import IntSet._

val tr1 = new NonEmpty(2,
  new NonEmpty(1, Empty, Empty),
  new NonEmpty(3, Empty, new NonEmpty(4, Empty, Empty)))

val tr2 = new NonEmpty(5,
  new NonEmpty(4, Empty, new NonEmpty(6, Empty, Empty)),
  Empty
)

println(tr1)
println()
println(tr2)

val tr3 = tr1 union tr2
println()
println(tr3)