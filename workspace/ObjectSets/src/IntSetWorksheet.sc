
import generics._

object IntSetWorksheet {
  var t1 = new NonEmptySet(3, EmptySet, EmptySet) //> t1  : NonEmptySet = {. 3 .}
  var t2 = t1 incl 4                              //> t2  : IntSet = {. 3 {. 4 .}}

  val x = if (true) 1 else false                  //> x  : AnyVal = 1

  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)                      //> nth: [T](n: Int, xs: generics.List[T])T

  var l = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> l  : generics.Cons[Int] = generics.Cons@7f387985

  nth(0, l)                                       //> res0: Int = 1
  nth(2,l)                                        //> res1: Int = 3
  nth(-1,l)                                       //> java.lang.IndexOutOfBoundsException
                                                  //| 	at IntSetWorksheet$$anonfun$main$1.nth$1(IntSetWorksheet.scala:11)
                                                  //| 	at IntSetWorksheet$$anonfun$main$1.apply$mcV$sp(IntSetWorksheet.scala:19
                                                  //| )
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at IntSetWorksheet$.main(IntSetWorksheet.scala:4)
                                                  //| 	at IntSetWorksheet.main(IntSetWorksheet.scala)
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(that: IntSet): IntSet

  override def toString = "."
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this

  override def toString = "{" + left + " " + elem + " " + right + "}"
  def union(that: IntSet): IntSet = ((left union right) union that) incl elem
}

object EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, EmptySet, EmptySet)
  def union(that: IntSet): IntSet = that
}