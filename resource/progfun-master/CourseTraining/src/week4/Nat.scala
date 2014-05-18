package week4

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

/*
object Zero extends Nat{
   
}

class Succ:(n:Nat) extends Zero{
  
}*/