package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == 0 || r == 1 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def helper(chars: List[Char], brackets: Int): Boolean = {
      if (brackets < 0) false
      else if (chars.isEmpty) brackets == 0
      else if (chars.head == '(')
        helper(chars.tail, brackets + 1)
      else if (chars.head == ')')
        helper(chars.tail, brackets - 1)
      else
        helper(chars.tail, brackets)
    }

    helper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def helper(money: Int, coins: List[Int], acc: Int): Int = {
      if (money == 0)
        acc + 1
      else if (money < 0 || coins.isEmpty)
        acc
      else helper(money - coins.head, coins, acc) + helper(money, coins.tail, acc)
    }

    if (money <= 0 || coins.isEmpty)
      0
    else
      helper(money, coins, 0)
  }
}
