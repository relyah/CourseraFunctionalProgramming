package patmat

import Huffman._

object learning {
  var leafA = Leaf('a', 4)                        //> leafA  : patmat.Huffman.Leaf = Leaf(a,4)
  var leafB = Leaf('b', 5)                        //> leafB  : patmat.Huffman.Leaf = Leaf(b,5)

  var treeAB = makeCodeTree(leafA, leafB)         //> treeAB  : patmat.Huffman.Fork = Fork(Leaf(a,4),Leaf(b,5),List(a, b),9)

  var myString = "hello world"                    //> myString  : String = hello world
  var myStrList = string2Chars(myString)          //> myStrList  : List[Char] = List(h, e, l, l, o,  , w, o, r, l, d)
  times(myStrList)                                //> res0: List[(Char, Int)] = List((h,1), (e,1), (l,3), (o,2), ( ,1), (w,1), (r,
                                                  //| 1), (d,1))

  myStrList.filter(x => x != 'h')                 //> res1: List[Char] = List(e, l, l, o,  , w, o, r, l, d)
  myStrList.tail.count(x => x == 'h')             //> res2: Int = 0

  var myTimes = times(myStrList)                  //> myTimes  : List[(Char, Int)] = List((h,1), (e,1), (l,3), (o,2), ( ,1), (w,1)
                                                  //| , (r,1), (d,1))
  myTimes.sortWith((a, b) => a._2 < b._2)         //> res3: List[(Char, Int)] = List((h,1), (e,1), ( ,1), (w,1), (r,1), (d,1), (o,
                                                  //| 2), (l,3))

  var myCodeTree = createCodeTree(myStrList)      //> myCodeTree  : patmat.Huffman.CodeTree = Fork(Fork(Fork(Leaf(r,1),Leaf(d,1),L
                                                  //| ist(r, d),2),Fork(Leaf( ,1),Leaf(w,1),List( , w),2),List(r, d,  , w),4),Fork
                                                  //| (Leaf(l,3),Fork(Fork(Leaf(h,1),Leaf(e,1),List(h, e),2),Leaf(o,2),List(h, e, 
                                                  //| o),4),List(l, h, e, o),7),List(r, d,  , w, l, h, e, o),11)

  def myEncode = encode(myCodeTree)_              //> myEncode: => List[Char] => List[patmat.Huffman.Bit]
  var myBits = myEncode(myStrList)                //> myBits  : List[patmat.Huffman.Bit] = List(1, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 0
                                                  //| , 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1)
  decode(myCodeTree,myBits)                       //> res4: List[Char] = List(h, e, l, l, o,  , w, o, r, l, d)
}