import fin._

object mutablestatewc {

  def power(x: Double, n: Int): Double = {
    var r = 1.0
    var i = n
    var j = 0
    while (j < 32) {
      r = r * r
      //println(r)
      if (i < 0)
        r *= x
      i = i << 1
      //println(i)
      j += 1
    }
    r
  }                                               //> power: (x: Double, n: Int)Double

  def whileLoop(condition: => Boolean)(command: => Unit) {
    if (condition) {
      command; whileLoop(condition)(command)
    } else ()
  }                                               //> whileLoop: (condition: => Boolean)(command: => Unit)Unit

  def repeatLoop(command: => Unit)(condition: => Boolean) {
    command;
    whileLoop(condition)(command)
  }                                               //> repeatLoop: (command: => Unit)(condition: => Boolean)Unit
  
  

  def y: String = "hello"                         //> y: => String
  //y = "bye"

  var x: String = "hello"                         //> x  : String = hello

  x = "bye"

  println(x)                                      //> bye
  println(y)                                      //> hello

  val b1 = new BankAccount;                       //> b1  : fin.BankAccount = Balance= 0
  val b2 = new BankAccount;                       //> b2  : fin.BankAccount = Balance= 0
  b1.deposit(50)
  b1.deposit(10)

  println(b1)                                     //> Balance= 60

  power(2, 3)                                     //> res0: Double = 8.0

}