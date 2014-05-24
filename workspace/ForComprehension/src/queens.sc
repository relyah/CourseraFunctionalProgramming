object queens {
  def isSafe(col: Int, queens: List[Int], delta: Int): Boolean = {
    if (queens.isEmpty) true
    else {
      val x = queens.head
      if (col == x || col == x - delta || col == x + delta) false
      else isSafe(col, queens.tail, delta + 1)

    }
  }                                               //> isSafe: (col: Int, queens: List[Int], delta: Int)Boolean

  def queens(n: Int): List[List[Int]] = {
    def placeQueens(k: Int): List[List[Int]] =
      if (k == 0) List(List())
      else for {
        queens <- placeQueens(k - 1)
        column <- List.range(1, n + 1)
        if isSafe(column, queens, 1)
      } yield column :: queens
    placeQueens(n)
  }                                               //> queens: (n: Int)List[List[Int]]

  val sol = queens(4)                             //> sol  : List[List[Int]] = List(List(3, 1, 4, 2), List(2, 4, 1, 3))
  sol.length                                      //> res0: Int = 2

  val init = List(List())                         //> init  : List[List[Nothing]] = List(List())
  for {
    queens <- init
    column <- List.range(1, 4 + 1)
    if isSafe(column, queens, 1)
  } yield column :: queens                        //> res1: List[List[Int]] = List(List(1), List(2), List(3), List(4))

}