package recfun
import common._
import scala.util._
import scala.collection.generic.SeqFactory

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
  def pascal(c: Int, r: Int): Int = {
    def findAbove(currentRow: Int, currentColumn: Int): Int = {
      if (currentColumn == 0 || currentRow == 0 || currentColumn == currentRow)  1
      else findAbove(currentRow - 1, currentColumn - 1) + findAbove(currentRow - 1, currentColumn)
    }
    
    if (r > 0 && c > 0) {
      if (c == r)  1
      else  findAbove(r - 1, c - 1) + findAbove(r - 1, c)
    }else 1
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var numberOfP:Int = 0
    if (!chars.isEmpty) {
      
      chars.foreach((c: Char) => {
        if (c == '(' && numberOfP >= 0) numberOfP += 1
        if (c == ')') numberOfP -= 1
      })
    } else false
    (numberOfP == 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    var result: Int = 0
    if (coins.isEmpty) result

    def findDivider(number: Int, tempList: List[Int]): Unit = {
      if (number >0) {
        tempList.indices.foreach(i=> {
          val c = tempList(i)
          val temp = number - c
          val tempList2 = tempList.drop(i)
          if ( temp == 0) {
            result += 1
          } else if (temp > 0) {
            findDivider(temp, tempList2)
          }
        })
      }
    }

    findDivider(money, coins)

    result
  }
}
