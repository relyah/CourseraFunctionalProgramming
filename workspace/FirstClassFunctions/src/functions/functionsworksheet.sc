package functions

object functionsworksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def powerOfTwo(x: Int): Int = if (x == 0) 1 else 2 * powerOfTwo(x - 1)
                                                  //> powerOfTwo: (x: Int)Int
  def sumPowersOfTwo(a: Int, b: Int): Int =
    if (a > b) 0 else powerOfTwo(a) + sumPowersOfTwo(a + 1, b)
                                                  //> sumPowersOfTwo: (a: Int, b: Int)Int

  sumPowersOfTwo(1, 2)                            //> res0: Int = 6

  def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)     //> sum: (f: Int => Int)(a: Int, b: Int)Int

  def id(x: Int): Int = x                         //> id: (x: Int)Int
  val sumInts = sum(x => x) _                     //> sumInts  : (Int, Int) => Int = <function2>
  def sumInts2 = sum(id) _                        //> sumInts2: => (Int, Int) => Int

  sumInts(1, 2)                                   //> res1: Int = 3
  (sum(x => x * x))(1, 10)                        //> res2: Int = 385

  def myfunc(x: Int)(y: Int): Int = {
    def myInnerFunc(a: Int): Int = a + 1
    myInnerFunc(x) + y + 1
  }                                               //> myfunc: (x: Int)(y: Int)Int

  myfunc(1)(5)                                    //> res3: Int = 8

  def sumTR(f: Int => Int)(a: Int, b: Int): Int = {
    def iter(fa: Int, b: Int): Int = {
      println(fa, " ", b)
      if (a == b) {
        println("Final result ", fa)
        fa
      } else iter(fa + f(b), b - 1)
    }
    iter(f(a), b)
  }                                               //> sumTR: (f: Int => Int)(a: Int, b: Int)Int

  sumTR(x => x * x)(2, 4)                         //> (4, ,4)
                                                  //| (20, ,3)
                                                  //| (29, ,2)
                                                  //| (Final result ,29)
                                                  //| res4: Int = 29
  val mysum = sumTR(x => x)_                      //> mysum  : (Int, Int) => Int = <function2>
  mysum(1, 3)                                     //> (1, ,3)
                                                  //| (4, ,2)
                                                  //| (6, ,1)
                                                  //| (Final result ,6)
                                                  //| res5: Int = 6

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def iter(acc: Int, b: Int): Int = {
      if (a == b)
        acc
      else
        iter(acc * f(b), b - 1)
    }
    iter(f(a), b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  product(x=>x)(1,5)                              //> res6: Int = 120

  def genTR(f: Int => Int, opp: (Int, Int) => Int)(a: Int, b: Int): Int = {
    def iter(fa: Int, b: Int): Int = {
      println(fa, " ", b)
      if (a == b) {
        println("Final result ", fa)
        fa
      } else iter(opp(fa, f(b)), b - 1)
    }
    iter(f(a), b)
  }                                               //> genTR: (f: Int => Int, opp: (Int, Int) => Int)(a: Int, b: Int)Int

  genTR(x => x, (x, y) => x * y)(0, 0)            //> (0, ,0)
                                                  //| (Final result ,0)
                                                  //| res7: Int = 0

  def fixedpoint(x: Int) = x                      //> fixedpoint: (x: Int)Int

  fixedpoint(1)                                   //> res8: Int = 1

  type IntPairPred = (Int, Int) => Boolean
  //val gt: IntPairPred = _ > _
  val gt: (Int, Int) => Boolean = _ > _           //> gt  : (Int, Int) => Boolean = <function2>

  gt(5, 4)                                        //> res9: Boolean = true

  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) = Math.abs((x - y) / x) < tolerance
                                                  //> isCloseEnough: (x: Double, y: Double)Boolean
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      println(next)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double

  def sqrt(x: Double) = fixedPoint(averageDamp(a => x / a))(1.0)
                                                  //> sqrt: (x: Double)Double
  sqrt(2)                                         //> 1.5
                                                  //| 1.4166666666666665
                                                  //| 1.4142156862745097
                                                  //| 1.4142135623746899
                                                  //| res10: Double = 1.4142135623746899

  def cube(x: Double) = fixedPoint(averageDamp(a => x / (a * a)))(1.0)
                                                  //> cube: (x: Double)Double

  //cube(9)
  //cube(8)
  //cube(27)

}