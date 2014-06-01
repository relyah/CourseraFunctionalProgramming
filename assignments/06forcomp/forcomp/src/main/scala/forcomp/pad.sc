package forcomp

object pad {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val myString: String = "I love you"             //> myString  : String = I love you
  myString.groupBy(c => c.toLower).mapValues(l => l.length()).toList.sortBy(p => p._1)
                                                  //> res0: List[(Char, Int)] = List(( ,2), (e,1), (i,1), (l,1), (o,2), (u,1), (v,
                                                  //| 1), (y,1))

  val sentence = "I" :: "love" :: "you" :: Nil    //> sentence  : List[String] = List(I, love, you)
  sentence.reduce((a,b) => a + b)                 //> res1: String = Iloveyou
}