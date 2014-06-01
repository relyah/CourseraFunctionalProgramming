class FileAsIterable {
  def iterator = scala.io.Source.fromFile("/home/bert/code/scala/coursera/functionalprogramming/workspace/WhyScala/src/someFile.txt").getLines
}

import scala.language.dynamics
class MyMap extends Dynamic {
  def selectDynamic(fieldName: String) = map.get(fieldName)
  private val map = Map("foo" -> "1", "bar" -> 2)
}
object tests {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def loopTill(cond: => Boolean)(body: => Unit): Unit = {
    if (cond) {
      body
      loopTill(cond)(body)
    }
  }                                               //> loopTill: (cond: => Boolean)(body: => Unit)Unit

  var i = 10                                      //> i  : Int = 10
  loopTill(i > 0) {
    println(i)
    i -= 1
  }                                               //> 10
                                                  //| 9
                                                  //| 8
                                                  //| 7
                                                  //| 6
                                                  //| 5
                                                  //| 4
                                                  //| 3
                                                  //| 2
                                                  //| 1
  val name = "roBert"                             //> name  : String = roBert
  val hasUpperCase = name.exists(_.isUpper)       //> hasUpperCase  : Boolean = true
  println(hasUpperCase)                           //> true

  val newIterator = new FileAsIterable with Iterable[String]
                                                  //> newIterator  : FileAsIterable with Iterable[String] = tests(line 1, line 2, 
                                                  //| line 3, end)
  newIterator.foreach(line => println(line))      //> line 1
                                                  //| line 2
                                                  //| line 3
                                                  //| end

  val someMap = new MyMap                         //> someMap  : MyMap = MyMap@596fde80
  println(someMap.foo)                            //> Some(1)
}