object ListWorkSheet {

  //def mapFun(xs: List[Int], f: Int => Int): List[Int] =
  //  (xs.foldRight(Nil: List[Int]())) { (xs, x) => xs :: (f(x))}

  def pack(init: List[String]): List[List[String]] = {
    def packAcc(acc: List[List[String]], remaining: List[String]): List[List[String]] = {
      remaining match {
        case Nil => acc
        case x :: xs => {

          val (left, right) = remaining.span(y => y.compareTo(x) == 0)
          packAcc(acc ::: List(left), right)
        }
      }
    }
    packAcc(List(), init)
  }                                               //> pack: (init: List[String])List[List[String]]

  def encode(init: List[String]): List[(String, Int)] = {
    val packed = pack(init)
    packed.map(x => (x.head, x.length))
  }                                               //> encode: (init: List[String])List[(String, Int)]

  def removeAt(n: Int, xs: List[Int]): List[Int] =
    {
      def removeAtAcc(now: Int, remaining: List[Int]): List[Int] =
        {
          remaining match {
            case List() => List()
            case y :: ys =>
              if (now == n) ys
              else y :: removeAtAcc(now + 1, ys)
          }
        }
      removeAtAcc(0, xs)
    }                                             //> removeAt: (n: Int, xs: List[Int])List[Int]

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sum(ys)
  }                                               //> sum: (xs: List[Int])Int

  def forall(p: Int => Boolean, l: List[Int]): Boolean =
    l.filter(p).length == l.length                //> forall: (p: Int => Boolean, l: List[Int])Boolean

  def squareList(xs: List[Int]): List[Int] = xs match {
    case List() => Nil
    case y :: ys => y * y :: squareList(ys)
  }                                               //> squareList: (xs: List[Int])List[Int]

  def squareList2(xs: List[Int]): List[Int] =
    xs map (x => x * x)                           //> squareList2: (xs: List[Int])List[Int]

  def msort[A](less: (A, A) => Boolean)(xs: List[A]): List[A] = {
    def merge(xs1: List[A], xs2: List[A]): List[A] =
      if (xs1.isEmpty) xs2
      else if (xs2.isEmpty) xs1
      else if (less(xs1.head, xs2.head)) xs1.head :: merge(xs1.tail, xs2)
      else xs2.head :: merge(xs1, xs2.tail)
    val n = xs.length / 2
    if (n == 0) xs
    else merge(msort(less)(xs take n), msort(less)(xs drop n))
  }                                               //> msort: [A](less: (A, A) => Boolean)(xs: List[A])List[A]

  def init(l: List[Int]): List[Int] = l match {
    case Nil => error("Nil.init")
    case x :: Nil => Nil
    case x :: xs => x :: init(l.tail)
  }                                               //> init: (l: List[Int])List[Int]

  def lengthAcc(acc: Int, l: List[Int]): Int = l match {
    case Nil => acc
    case x :: xs => lengthAcc(acc + 1, xs)
  }                                               //> lengthAcc: (acc: Int, l: List[Int])Int

  def lengthTR(l: List[Int]): Int =
    lengthAcc(0, l)                               //> lengthTR: (l: List[Int])Int

  def insert(head: Int, l: List[Int]): List[Int] =
    if (l.isEmpty) List(head)
    else if (head <= l.head) head :: l
    else
      l.head :: insert(head, l.tail)              //> insert: (head: Int, l: List[Int])List[Int]

  def isort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))          //> isort: (xs: List[Int])List[Int]

  var fruit: List[String] = List("apples", "oranges", "pears")
                                                  //> fruit  : List[String] = List(apples, oranges, pears)
  var f2 = List("a", "b", "c")                    //> f2  : List[String] = List(a, b, c)
  var nums = 5 :: 12 :: 1 :: 4 :: 67 :: Nil       //> nums  : List[Int] = List(5, 12, 1, 4, 67)
  println(fruit)                                  //> List(apples, oranges, pears)
  println(f2)                                     //> List(a, b, c)
  println(nums)                                   //> List(5, 12, 1, 4, 67)

  nums.head                                       //> res0: Int = 5
  nums.tail                                       //> res1: List[Int] = List(12, 1, 4, 67)

  isort(nums)                                     //> res2: List[Int] = List(1, 4, 5, 12, 67)
  lengthTR(nums)                                  //> res3: Int = 5
  init(nums)                                      //> res4: List[Int] = List(5, 12, 1, 4)
  msort((l: Int, r: Int) => l < r)(nums)          //> res5: List[Int] = List(1, 4, 5, 12, 67)
  squareList(nums)                                //> res6: List[Int] = List(25, 144, 1, 16, 4489)
  squareList2(nums)                               //> res7: List[Int] = List(25, 144, 1, 16, 4489)
  forall(x => x < 60, nums)                       //> res8: Boolean = false
  sum(nums)                                       //> res9: Int = 89
  nums reduceRight { (x, y) => x * y }            //> res10: Int = 16080
  (nums foldRight 0) { (x, y) => x + y }          //> res11: Int = 89
  (0 /: nums) { (x, y) => x + y }                 //> res12: Int = 89
  (nums :\ 0) { (x, y) => x + y }                 //> res13: Int = 89
  var x = 1 :: 2 :: 3 :: Nil                      //> x  : List[Int] = List(1, 2, 3)
  x.head                                          //> res14: Int = 1

  var nums2 = 1 :: 2 :: 3 :: 44 :: Nil            //> nums2  : List[Int] = List(1, 2, 3, 44)

  nums :: nums2                                   //> res15: List[Any] = List(List(5, 12, 1, 4, 67), 1, 2, 3, 44)
  nums.::(nums2)                                  //> res16: List[Any] = List(List(1, 2, 3, 44), 5, 12, 1, 4, 67)
  nums ::: nums2                                  //> res17: List[Int] = List(5, 12, 1, 4, 67, 1, 2, 3, 44)
  nums.reduceRight((x, y) => x + y)               //> res18: Int = 89
  nums.foldLeft(0)((x, y) => x + y)               //> res19: Int = 89
  nums.foldLeft(1)((x, y) => x * y)               //> res20: Int = 16080

  val newList = 4 :: nums2                        //> newList  : List[Int] = List(4, 1, 2, 3, 44)
  val newList2 = nums2 ::: List(4)                //> newList2  : List[Int] = List(1, 2, 3, 44, 4)
  var newList3 = nums2 ++ List(4)                 //> newList3  : List[Int] = List(1, 2, 3, 44, 4)

  removeAt(1, nums2)                              //> res21: List[Int] = List(1, 3, 44)
  var pair = ("hello", 42)                        //> pair  : (String, Int) = (hello,42)
  var (label, value) = pair                       //> label  : String = hello
                                                  //| value  : Int = 42
  println(label)                                  //> hello

  var l1 = List(1, 2)                             //> l1  : List[Int] = List(1, 2)
  var l2 = List(3, 4)                             //> l2  : List[Int] = List(3, 4)
  var l3 = l1 :: List(l2)                         //> l3  : List[List[Int]] = List(List(1, 2), List(3, 4))
  l3 ::: List(l2)                                 //> res22: List[List[Int]] = List(List(1, 2), List(3, 4), List(3, 4))

  pack(List("a", "a", "a", "b", "c", "c", "a"))   //> res23: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a
                                                  //| ))
  encode(List("a", "a", "a", "b", "c", "c", "a")) //> res24: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
}