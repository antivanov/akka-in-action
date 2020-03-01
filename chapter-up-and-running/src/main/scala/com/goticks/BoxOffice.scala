package com.goticks

import scala.concurrent.Future

import akka.actor._
import akka.util.Timeout

object BoxOffice {
  def props(implicit timeout: Timeout) = Props(new BoxOffice)
  def name = "boxOffice"

  case class EventName(value: String) extends AnyVal
  case class Event(name: EventName, tickets: Int)
  case class Events(events: Vector[Event])

  sealed trait EventResponse
  case class EventCreated(event: Event) extends EventResponse
  case object EventExists extends EventResponse

  sealed trait BoxOfficeMessage
  case class CreateEvent(eventName: EventName, tickets: Int) extends BoxOfficeMessage
  case class GetTickets(eventName: EventName, tickets: Int) extends BoxOfficeMessage
  case class GetEvent(eventName: EventName) extends BoxOfficeMessage
  case object GetEvents extends BoxOfficeMessage
  case class CancelEvent(eventName: EventName) extends BoxOfficeMessage

}

class BoxOffice(implicit timeout: Timeout) extends Actor {
  import BoxOffice._
  import context._


  def createTicketSeller(name: EventName) =
    context.actorOf(TicketSeller.props(name.value), name.value)

  def receive = {
    case CreateEvent(eventName, tickets) =>
      def create() = {
        val eventTickets = createTicketSeller(eventName)
        val newTickets = (1 to tickets).map { ticketId =>
          TicketSeller.Ticket(ticketId)
        }.toVector
        eventTickets ! TicketSeller.Add(newTickets)
        sender() ! EventCreated(Event(eventName, tickets))
      }
      context.child(eventName.value).fold(create())(_ => sender() ! EventExists)



    case GetTickets(eventName, tickets) =>
      def notFound() = sender() ! TicketSeller.Tickets(eventName)
      def buy(child: ActorRef) =
        child.forward(TicketSeller.Buy(tickets))

      context.child(eventName.value).fold(notFound())(buy)


    case GetEvent(eventName) =>
      def notFound() = sender() ! None
      def getEvent(child: ActorRef) = child forward TicketSeller.GetEvent
      context.child(eventName.value).fold(notFound())(getEvent)


    case GetEvents =>
      import akka.pattern.ask
      import akka.pattern.pipe

      def getEvents = context.children.map { child =>
        self.ask(GetEvent(EventName(child.path.name))).mapTo[Option[Event]]
      }
      def convertToEvents(f: Future[Iterable[Option[Event]]]) =
        f.map(events => Events(events.flatten.toVector))

      pipe(convertToEvents(Future.sequence(getEvents))) to sender()


    case CancelEvent(eventName) =>
      def notFound() = sender() ! None
      def cancelEvent(child: ActorRef) = child forward TicketSeller.Cancel
      context.child(eventName.value).fold(notFound())(cancelEvent)
  }
}
