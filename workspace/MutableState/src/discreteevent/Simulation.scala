package discreteevent

package object discreteevent { type Action = () => Unit }

abstract class Simulation {

  def currentTime: Int
  def afterDelay(delay: Int, action: => discreteevent.Action)
  //def run()

  case class WorkItem(time: Int, action: discreteevent.Action)
  private type Agenda = List[WorkItem]
  private var agenda: Agenda = List()
  private var curtime = 0

  private def insert(ag: Agenda, item: WorkItem): Agenda =
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)

  def afterDelay(delay: Int)(block: => Unit) {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() {
    agenda match {
      case WorkItem(time, action) :: rest =>
        agenda = rest;
        curtime = time;
        action()
      case List() =>
    }
  }

  def run() {
    afterDelay(0) {
      println("*** simulation started ***")
      while (!agenda.isEmpty)
        next()
    }
  }

}