package classAndObject

object EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)
  def union(other: IntSet): IntSet = other
  
  override def toString = "empty"
}