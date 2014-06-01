object ListReduceFold {
  def sum(xs: List[Int]) = (0 :: xs) reduceLeft(_ + _)
                                                  //> sum: (xs: List[Int])Int
  def sumR(xs: List[Int]) = (0 :: xs) reduceRight(_ + _)
                                                  //> sumR: (xs: List[Int])Int
  sum(Nil)                                        //> res0: Int = 0
  sumR(1::2::3::4::Nil)                           //> res1: Int = 10
  
  val l = 1::2::33::4::Nil                        //> l  : List[Int] = List(1, 2, 33, 4)
  def fl = l.foldLeft(0)_                         //> fl: => ((Int, Int) => Int) => Int
  fl(_ * _)                                       //> res2: Int = 0
  
  (0::l)                                          //> res3: List[Int] = List(0, 1, 2, 33, 4)
  
  
}