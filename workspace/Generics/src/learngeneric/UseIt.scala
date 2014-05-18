package learngeneric

case class Num(value: Double) extends Ordered[Num] {
  def compare(that: Num): Int =
    if (this.value < that.value) -1
    else if (this.value > that.value) 1
    else 0
}

object UseIt {

  def main(args: Array[String]): Unit = {

    def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean = {
      p.isEmpty ||
        p.top == s.top && isPrefix[A](p.pop, s.pop)
    }

    val x = new EmptyStack[Int]
    val y = x.push(1).push(2)
    println(y.pop.top)

    val s1 = new EmptyStack[String].push("abc")
    val s2 = new EmptyStack[String].push("abx").push(s1.top)
    println(isPrefix(s1, s2))

    val s = new EmptySet[Num].incl(Num(1.0)).incl(Num(2.0))
    println(s.contains(Num(2.0)))

    def divmod(x: Int, y: Int) = new Tuple2[Int, Int](x / y, x % y)

    val dm = divmod(7, 5);
    println(dm._1)
    println(dm._2)
  }

}