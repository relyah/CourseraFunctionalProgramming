package caseAndPattern

//import .Expr

//class Number(n: Int) extends Expr {
////def isNumber: Boolean = true
////def isSum: Boolean = false
////def numValue: Int = n
////def leftOp: Expr = error("Number.leftOp")
////def rightOp: Expr = error("Number.rightOp")
//  def eval: Int = n
//}

case class Number(n: Int) extends Expr