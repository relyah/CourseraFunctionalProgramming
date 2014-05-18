package week4

trait List[T] {
	def isEmpty:Boolean
	def head:T
	def tail:List[T]
}

class Nil[T] extends List[T]{
	def isEmpty:Boolean = true
	def head = throw new NoSuchElementException("head")
	def tail = throw new NoSuchElementException("tail")
	
	override def toString:String = "_"
}

class Cons[T](val head: T , val tail:List[T]) extends List[T]{
    def isEmpty:Boolean = false
    
    override def toString:String = head + " "+ tail 
}
