package week4

abstract class B {
	def ifThenElse[T](t: => T, e: =>T):T
	
	def && (x: => B): B = ifThenElse(x, f)
	def || (x: => B): B = ifThenElse(t, x)
	def unary_! : B 			= ifThenElse(f,t)
	
	def == (x:B): B		= ifThenElse(x, x.unary_!)
	def != (x:B): B		= ifThenElse(x.unary_!,x)
}

object t extends B {
  def ifThenElse[T](t: =>T, e: =>T)= t
}

object f extends B {
  def ifThenElse[T](t: =>T, e: =>T)= e
}