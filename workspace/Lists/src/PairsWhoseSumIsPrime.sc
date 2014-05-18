object PairsWhoseSumIsPrime {

  var n: Int = 7                                  //> n  : Int = 7
  var one2n = List.range(1, n)                    //> one2n  : List[Int] = List(1, 2, 3, 4, 5, 6)
  var temp = one2n.map(i => List.range(1, i))     //> temp  : List[List[Int]] = List(List(), List(1), List(1, 2), List(1, 2, 3), L
                                                  //| ist(1, 2, 3, 4), List(1, 2, 3, 4, 5))
  var pairs = one2n.map(i => List.range(1, i).map(x => (i, x)))
                                                  //> pairs  : List[List[(Int, Int)]] = List(List(), List((2,1)), List((3,1), (3,2
                                                  //| )), List((4,1), (4,2), (4,3)), List((5,1), (5,2), (5,3), (5,4)), List((6,1),
                                                  //|  (6,2), (6,3), (6,4), (6,5)))
  var folded = pairs.foldRight(List[(Int, Int)]()) { (xs, ys) => xs ::: ys }
                                                  //> folded  : List[(Int, Int)] = List((2,1), (3,1), (3,2), (4,1), (4,2), (4,3), 
                                                  //| (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,3), (6,4), (6,5))

  List(1, 2, 3) map (_ + 2)                       //> res0: List[Int] = List(3, 4, 5)
  List(1, 2, 3) map (x => x + 2)                  //> res1: List[Int] = List(3, 4, 5)

  var l = List(("a", 1), ("b", 2), ("c", 3))      //> l  : List[(String, Int)] = List((a,1), (b,2), (c,3))

  l :+ ("d", 4)                                   //> res2: List[(String, Int)] = List((a,1), (b,2), (c,3), (d,4))
  List.concat(l, List(("d", 4)))                  //> res3: List[(String, Int)] = List((a,1), (b,2), (c,3), (d,4))
  List('a','c','d') ::: List('e','f')             //> res4: List[Char] = List(a, c, d, e, f)
  List.concat( List('a','c','d') ,List('e','f')  )//> res5: List[Char] = List(a, c, d, e, f)
}