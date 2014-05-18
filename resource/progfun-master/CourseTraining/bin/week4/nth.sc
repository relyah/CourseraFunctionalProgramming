import week4._

object nth {
	def singleton[T](elem:T) = new Cons[T](elem, new Nil[T])
                                                  //> singleton: [T](elem: T)week4.Cons[T]
	
	def nth[T](n:Int,xs: List[T]): T =
	  if ( xs.isEmpty) throw new IndexOutOfBoundsException("xs.isEmpty")
		else if ( n == 0) xs.head
		else nth(n - 1, xs.tail)          //> nth: [T](n: Int, xs: week4.List[T])T
	
	   
	val a = singleton(3, singleton(5))        //> a  : week4.Cons[(Int, week4.Cons[Int])] = (3,5 _) _
	
	nth(0, a)                                 //> res0: (Int, week4.Cons[Int]) = (3,5 _)
}