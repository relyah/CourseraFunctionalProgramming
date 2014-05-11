package caseAndPattern

object MainObject {

  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(l, r) => eval(l) + eval(r)
    case Prod(l, r) => eval(l) * eval(r)
  }

  def main(args: Array[String]): Unit = {

    println(eval(Sum(Number(1), Number(2))))
    println(eval(Prod(Number(3), Number(2))))
  }

}