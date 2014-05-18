object rationals {
/*
  val x = new Rational(1,2)
  x.number
 
  
  val z = x.add (new Rational(2,3) )
  z.number
  z.denom
  
  val y = z + new Rational(2,4)
  val yy = y - new Rational(2,4)

  yy.neg
  
  */
  val x = new Rational(1,3)                       //> x  : Rational = 1/3
  val y = new Rational(5,7)                       //> y  : Rational = 5/7
  val z = new Rational(3,2)                       //> z  : Rational = 3/2
  
  val t = x - y - z                               //> t  : Rational = -79/42
  y + y                                           //> res0: Rational = 10/7
  
  x < y                                           //> res1: Boolean = true
  y < x                                           //> res2: Boolean = false
  
  x max y                                         //> res3: Rational = 5/7
  y max z                                         //> res4: Rational = 3/2
  z max x                                         //> res5: Rational = 3/2
 
 new Rational(3)                                  //> res6: Rational = 3/1
}

class Rational(x:Int, y:Int){
   require(y != 0, "denom must not equal zero")

  private def gcd(a:Int, b:Int):Int = if ( b == 0) a else gcd(b, a % b)
  
  def this(x:Int) = this(x,1)
	
	def number = x / gcd(x,y)
	def denom = y / gcd(x,y)
	
	def < (that:Rational) = number * that.denom < that.number * denom
	
	def max(that:Rational) = if ( this <  that) that else this
	
	def unary_- : Rational = new Rational(-1 * number, denom)
	
	
	// def add(s:Rational):Rational =  new Rational(number * s.denom + s.number* denom, denom * s.denom)
	def + (s:Rational):Rational = new Rational(number * s.denom + s.number* denom, denom * s.denom)
	
	// def sub(s:Rational): Rational = add(s.neg)
	def - (that:Rational):Rational = this + -that
	
	override def toString = number + "/" + denom
	
}