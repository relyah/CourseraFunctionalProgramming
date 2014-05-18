object prime {
    def isPrime(n:Int): Boolean = (2 until n) forall (d => n % d !=0)
                                                  //> isPrime: (n: Int)Boolean
    
    isPrime(8)                                    //> res0: Boolean = false
    isPrime(7)                                    //> res1: Boolean = true
    isPrime(97)                                   //> res2: Boolean = true
}