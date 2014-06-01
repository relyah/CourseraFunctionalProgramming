package pacstrm

object Streamwc {

  def isPrime(n: Int) =
    List.range(2, n) forall (x => n % x != 0)     //> isPrime: (n: Int)Boolean

  def range(start: Int, end: Int): Stream[Int] =
    if (start >= end) Stream.empty
    else Stream.cons(start, range(start + 1, end))//> range: (start: Int, end: Int)Stream[Int]

  def print[A](xs: Stream[A]) {
    if (!xs.isEmpty) { Console.println(xs.head); print(xs.tail) }
  }                                               //> print: [A](xs: Stream[A])Unit

  val x = range(1, 5)                             //> x  : Stream[Int] = Stream(1, ?)
  print(x)                                        //> 1
                                                  //| 2
                                                  //| 3
                                                  //| 4

  Stream.cons(1, Stream.cons(2, Stream.empty))    //> res0: Stream.Cons[Int] = Stream(1, ?)
  
  Stream.range(1000, 10000).filter(p=>isPrime(p))(1)
                                                  //> res1: Int = 1013
  // filter isPrime at 1

}