package discreteevent

class OrGate {
  
    def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output setSignal (a1Sig | a2Sig)
      }
    }

    a1 addAction orAction
    a2 addAction orAction
  }

}