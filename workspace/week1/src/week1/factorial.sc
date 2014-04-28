package week1

object factorial {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def factorial(x: Int): Int = {
    def helper(x: Int, y: Int) : Int= {
      if (x == 0) y else helper(x - 1, y*x)
    }
    helper(x,1)
  }                                               //> factorial: (x: Int)Int

  factorial(2)                                    //> res0: Int = 2
  factorial(3)                                    //> res1: Int = 6

}