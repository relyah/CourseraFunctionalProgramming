package discreteevent

class Inverter {

  def inverter(input: Wire, output: Wire) {

    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !inputSig
      }
    }

    input addAction invertAction
  }

}