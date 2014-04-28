package squareroot

object squareroot {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def sqrt(x: Double) = {
    def abs(x: Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.1

    def improve(guess: Double) =
      (guess + x / guess) / 2
    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(1e-6)                                      //> res0: Double = 0.0010338412392442034
  sqrt(1e60)                                      //> res1: Double = 1.0126366141999421E30

}