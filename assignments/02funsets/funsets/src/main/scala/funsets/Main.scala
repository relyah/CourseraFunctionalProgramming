package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(FunSets.toString(singletonSet(1)))
  
   def s123: Set = x => x >= 1 && x <= 3
  println(FunSets.toString(s123))
}
