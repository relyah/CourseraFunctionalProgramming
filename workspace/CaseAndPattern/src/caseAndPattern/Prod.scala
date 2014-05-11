package caseAndPattern

//class class Prod extends Expr {
//  def eval: Int = e1.eval * e2.eval
//}

case class Prod(e1: Expr, e2: Expr) extends Expr