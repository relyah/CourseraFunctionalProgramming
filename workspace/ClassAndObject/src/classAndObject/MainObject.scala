package classAndObject

object MainObject {

  def main(args: Array[String]): Unit = {
    var i = 1
    var x = new Rational(0, 1)
    while (i <= 10) {
      x += new Rational(1, i)
      i += 1
    }
    println("" + x + " " + x.numer + "/" + x.denom)
    println("square: " + x.square)

    val mySet: IntSet = EmptySet
    val newSet = mySet.incl(5)
    println(mySet.contains(5))
    println(newSet.contains(5))

    val newSet2 = newSet.incl(6);
    val newSet3 = newSet.incl(7);

    val unionSet = newSet2.union(newSet3);

    println(unionSet)

  }

}