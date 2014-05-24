import forcomp._

object forcompwc {

  var mb = new MyBook()                           //> mb  : forcomp.MyBook = forcomp.MyBook@c2909a1
  mb.isEmpty                                      //> res0: Boolean = true

  case class Book(title: String, authors: List[String])

  def isPrime(n: Int) =
    List.range(2, n) forall (x => n % x != 0)     //> isPrime: (n: Int)Boolean

  def removeDuplicates[A](xs: List[A]): List[A] =
    if (xs.isEmpty) xs
    else xs.head :: removeDuplicates(for (x <- xs.tail if x != xs.head) yield x)
                                                  //> removeDuplicates: [A](xs: List[A])List[A]

  def flatten[A](xss: List[List[A]]): List[A] =
    (xss :\ (Nil: List[A]))((xs, ys) => xs ::: ys)//> flatten: [A](xss: List[List[A]])List[A]

  var xs = 1 :: 2 :: 3 :: 4 :: Nil                //> xs  : List[Int] = List(1, 2, 3, 4)
  var ys = 5 :: 6 :: 7 :: 8 :: Nil                //> ys  : List[Int] = List(5, 6, 7, 8)
  xs zip ys                                       //> res1: List[(Int, Int)] = List((1,5), (2,6), (3,7), (4,8))
  for ((x, y) <- xs zip ys) yield x * y           //> res2: List[Int] = List(5, 12, 21, 32)

  var n = 7                                       //> n  : Int = 7
  for (i <- List.range(1, n); j <- List.range(1, i); if isPrime(i + j)) yield (i, j)
                                                  //> res3: List[(Int, Int)] = List((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5
                                                  //| ))

  for {
    i <- List.range(1, n)
    j <- List.range(1, i)
    if isPrime(i + j)
  } yield (i, j)                                  //> res4: List[(Int, Int)] = List((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5
                                                  //| ))

  val books: List[Book] = List(
    Book("Structure and Interpretation of Computer Programs", List("Abelson, Harold", "Sussman, Gerald J.")),
    Book("Principles of Compiler Design", List("Aho, Alfred", "Ullman, Jeffrey")),
    Book("Programming in Modula-2", List("Wirth, Niklaus")),
    Book("Introduction to Functional Programming", List("Bird, Richard")),
    Book("The Java Language Specification",
      List("Gosling, James", "Joy, Bill", "Steele, Guy", "Bracha, Gilad")),
    Book("Title 1", List("Gosling, James")))      //> books  : List[forcompwc.Book] = List(Book(Structure and Interpretation of C
                                                  //| omputer Programs,List(Abelson, Harold, Sussman, Gerald J.)), Book(Principle
                                                  //| s of Compiler Design,List(Aho, Alfred, Ullman, Jeffrey)), Book(Programming 
                                                  //| in Modula-2,List(Wirth, Niklaus)), Book(Introduction to Functional Programm
                                                  //| ing,List(Bird, Richard)), Book(The Java Language Specification,List(Gosling
                                                  //| , James, Joy, Bill, Steele, Guy, Bracha, Gilad)), Book(Title 1,List(Gosling
                                                  //| , James)))

  for (b <- books; a <- b.authors if a startsWith "Ullman") yield b.title
                                                  //> res5: List[String] = List(Principles of Compiler Design)

  var l = List("Abelson, Harold", "Sussman, Gerald J.")
                                                  //> l  : List[String] = List(Abelson, Harold, Sussman, Gerald J.)
  l.filter(a => a.startsWith("A")).isEmpty        //> res6: Boolean = false

  for (b <- books if !b.authors.filter(a => a.startsWith("Ullman")).isEmpty) yield b.title
                                                  //> res7: List[String] = List(Principles of Compiler Design)
  var subList = for (
    b1 <- books; b2 <- books if b1 != b2;
    a1 <- b1.authors; a2 <- b2.authors if a1 == a2
  ) yield a1                                      //> subList  : List[String] = List(Gosling, James, Gosling, James)
  removeDuplicates(subList)                       //> res8: List[String] = List(Gosling, James)

  var lofl = List(List(1, 2, 3), List(-1, -2, -3), List(4, 5, 6))
                                                  //> lofl  : List[List[Int]] = List(List(1, 2, 3), List(-1, -2, -3), List(4, 5, 
                                                  //| 6))
  flatten(lofl)                                   //> res9: List[Int] = List(1, 2, 3, -1, -2, -3, 4, 5, 6)
  for (x <- lofl; y <- x) yield y                 //> res10: List[Int] = List(1, 2, 3, -1, -2, -3, 4, 5, 6)
  for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title
                                                  //> res11: List[String] = List(Introduction to Functional Programming)
  books.filter(x => (!x.authors.filter(y => y.startsWith("Bird")).isEmpty)).map(x => x.title)
                                                  //> res12: List[String] = List(Introduction to Functional Programming)

  for (b <- books if (b.title indexOf "Program") >= 0) yield b.title
                                                  //> res13: List[String] = List(Structure and Interpretation of Computer Program
                                                  //| s, Programming in Modula-2, Introduction to Functional Programming)
  for (ls <- lofl) {
    for (v <- ls) {
      print(v + "\t")
    }
    println()
  }                                               //> 1	2	3	
                                                  //| -1	-2	-3	
                                                  //| 4	5	6	
  lofl.foreach(ls => {
    ls.foreach(v => print(v + "\t"));
    println()
  })                                              //> 1	2	3	
                                                  //| -1	-2	-3	
                                                  //| 4	5	6	
}