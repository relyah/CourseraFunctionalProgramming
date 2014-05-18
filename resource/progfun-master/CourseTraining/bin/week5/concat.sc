object concat {
  
  def concat[T](xs:List[T], ys:List[T]): List[T] =
  	(xs foldRight ys) (_ :: _)                //> concat: [T](xs: List[T], ys: List[T])List[T]
  	
  val a = List(2,3,1)                             //> a  : List[Int] = List(2, 3, 1)
  val b = List(6,3)                               //> b  : List[Int] = List(6, 3)
  
  concat(a,b)                                     //> res0: List[Int] = List(2, 3, 1, 6, 3)
}