package generics

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}