package forcomp


object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /*
  * count a char in a word
   */
  /*
  def count(ch:Char, w:Word):Int = {
    var count:Int = 0

    w.count(p => ch==p)

    for ( c <- w; ch == c) count += 1

    count
  }                    */

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.groupBy(_.toLower).mapValues(_.length).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(sentence: Sentence): Occurrences = sentence match {
    case Nil => Nil
    case head :: tail => wordOccurrences(sentence.reduceLeft(_ + _)).sortBy(_._1)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =  dictionary.map(w => (wordOccurrences(w), w)).groupBy(_._1).mapValues(words => words.map(_._2))


  def without(index: Int, word: String): String = new StringBuilder(word).deleteCharAt(index).toString



  /*
  def anagram (word: Word): List[Word] = {
    if (word.length == 1) {
      List(word)
    } else {
      var anagrams = ListBuffer[String]()
      0 to word.length-1 foreach { i =>
        anagrams ++=  (anagram(without(i, word)) map (word.charAt(i) + _))
      }
      anagrams.toList
    }
  }
      */
  /** Returns all the anagrams of a given word. Filtre by dictionary */
  def wordAnagrams(word: Word): List[Word] =  dictionaryByOccurrences.find(_._1 == wordOccurrences(word)).map(_._2) getOrElse Nil


  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def loop0(elt: (Char, Int)): Occurrences = elt match {
      case (char, count) =>
        (for {
          index <- 1 to count
        } yield char -> index).toList
    }

    def loop1(elt: List[Occurrences], rest: List[Occurrences], deep: Int): List[Occurrences] = {
      (elt, rest) match {
        case (head :: tail, _) if(head.length == deep) => elt
        case (acc, tail) => {
          val res = for {
            t <- tail
            x <- t
            y <- elt
            if(!y.contains(x))
          } yield {
            (y :+ x)
          }
          if(tail.length > 1) loop1(res, tail.tail, deep) else loop1(res, tail, deep)
        }
      }
    }

    def loop2(remain: Occurrences, occ: Occurrences, acc: List[Occurrences]): List[Occurrences] = occ match {
      case Nil => acc
      case (head :: tail) => {
        val res = (for {
          deep <- 1 to occurrences.length
          cb <- loop0(head)
        } yield loop1(List(List(cb)), tail.map(loop0), deep)
          ).reduceLeft(_ ++ _).toList
        if(!remain.contains(head)) {
          loop2(head :: remain, tail :+ head, acc ++ res)
        }
        else acc
      }
    }
    loop2(Nil, occurrences, Nil).map(_.sortBy(identity)).distinct ++ List(Nil)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(xs: Occurrences, ys: Occurrences): Occurrences =  {
    def loop(acc: Map[Char, Int], elts: Occurrences): Occurrences = elts match {
      case Nil => {
        acc.toList.sortBy(_._1)
      }
      case (char, c1) :: tail => acc.get(char).map { c2 =>
        if(c2 - c1 <= 0) loop(acc - char, tail) else loop(acc updated (char, c2 - c1), tail)
      }  getOrElse loop(acc, tail)
    }
    loop(xs.toMap, ys)
  }
  /* for {
         xx <- x
         yy <- y
         if xx._1 != yy._1
         if xx._2 <= yy._2 } yield xx
             */

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] ={
    def sentenceAnagrams0(all: Occurrences, occurrences: Occurrences, sentence: Sentence): List[Sentence] = {
      if(occurrences.toMap.get('a').isDefined && occurrences.toMap.get('r').isDefined && occurrences.toMap.get('t').isDefined) println(all, occurrences, sentence)
      combinations(occurrences).flatMap { occ =>
        if(occ.toMap.get('a').isDefined && occ.toMap.get('r').isDefined && occ.toMap.get('t').isDefined) println(all, occurrences, occ, sentence)
        val words = dictionaryByOccurrences.get(occ) getOrElse Nil
        (words, occ, all) match {
          case (_, _, Nil) => {
            List(sentence)
          }
          case (words, occ, _) => {
            (for {
              word <- words
              s <- sentenceAnagrams0(subtract(all, occ), subtract(all, occ), sentence :+ word)
            } yield s)
          }
        }
      }
    }
    val all = sentenceOccurrences(sentence)
    sentenceAnagrams0(all, all, Nil)
  }

}
