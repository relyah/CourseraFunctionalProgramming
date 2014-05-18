object ClassesHierarcy {
	val t1 = new NonEmpty(3, new Empty, new Empty)
                                                  //> t1  : NonEmpty = {. 3 .}
	val t2 =  t1 incl 4                       //> t2  : IntSet = {. 3 {. 4 .}}
  val t3 = t1 incl 1                              //> t3  : IntSet = {{. 1 .} 3 .}
  val u = t1  union t2                            //> u  : IntSet = {. 3 {. 4 .}}
  
  
  val n1 = null                                   //> n1  : Null = null
  val n2:String = null                            //> n2  : String = null

  if(true) 1 else false                           //> res0: AnyVal = 1
}

abstract class IntSet {
	def incl(x:Int) : IntSet
	def contains(x: Int): Boolean
	def union(other: IntSet): IntSet
}


class Empty extends IntSet {
	def incl(x:Int) : IntSet = new NonEmpty(x, new Empty, new Empty)
	def contains(x:Int): Boolean  = false
	def union(other: IntSet): IntSet = other
	
  override def toString = "."
}


class NonEmpty(elem:Int, left:IntSet, right:IntSet) extends IntSet {
	def incl(x:Int) : IntSet = {
		if ( x < elem) new NonEmpty( elem, left incl x, right)
		else if ( x > elem)  new NonEmpty( elem, left, right incl x)
		else this
	}
	
	def contains(x:Int): Boolean  = {
		if ( x  < elem )  left contains x
		else if ( x > elem) right contains x
		else true
	}
	
	def union(other:IntSet): IntSet = (( left union right) union other) incl elem
	
	override def toString = "{" + left + " "+ elem + " " + right + "}"
}