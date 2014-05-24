package discreteevent

class Wire {
  private var sigVal = false
  private var actions: List[discreteevent.Action] = List()
  def getSignal = sigVal
  def setSignal (s: Boolean) =
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  def addAction(a: discreteevent.Action) {
    actions = a :: actions; a()
  }
}