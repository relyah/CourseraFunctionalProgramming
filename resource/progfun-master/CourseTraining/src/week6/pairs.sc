object pairs {
 	val n = 7                                 //> n  : Int = 7
 	
 	def isPrime(n:Int): Boolean = (2 until n) forall (d => n % d !=0)
                                                  //> isPrime: (n: Int)Boolean
 	
 	def calculatePrime(n:Int) =
 		(1 until n) flatMap ( i =>
 			(1 until i) map (j => (i,j))) filter ( pair =>
 				(isPrime(pair._1 + pair._2)))
                                                  //> calculatePrime: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
   
   calculatePrime(7)                              //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
            
            
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = (for ( (x,y) <- xs zip ys) yield x * y).sum
                                                  //> scalarProduct: (xs: List[Double], ys: List[Double])Double
  
  
  scalarProduct(List(2,3,4), List(2,1,3))         //> res1: Double = 19.0
}