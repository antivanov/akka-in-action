package com.goticks

import akka.actor.{Actor, PoisonPill, Props}
import com.goticks.BoxOffice.EventName

object TicketSeller {
  def props(event: String) = Props(new TicketSeller(EventName(event)))

  case class Ticket(id: Int)
  case class Tickets(event: EventName,
                     entries: Vector[Ticket] = Vector.empty[Ticket])

  sealed trait TicketSellerMessage
  case class Add(tickets: Vector[Ticket]) extends TicketSellerMessage
  case class Buy(tickets: Int) extends TicketSellerMessage
  case object GetEvent extends TicketSellerMessage
  case object Cancel extends TicketSellerMessage
}

class TicketSeller(event: EventName) extends Actor {
  import TicketSeller._

  var tickets = Vector.empty[Ticket]

  def receive = {
    case Add(newTickets) => tickets = tickets ++ newTickets
    case Buy(nrOfTickets) =>
      val entries = tickets.take(nrOfTickets)
      if(entries.size >= nrOfTickets) {
        sender() ! Tickets(event, entries)
        tickets = tickets.drop(nrOfTickets)
      } else sender() ! Tickets(event)
    case GetEvent => sender() ! Some(BoxOffice.Event(event, tickets.size))
    case Cancel =>
      sender() ! Some(BoxOffice.Event(event, tickets.size))
      self ! PoisonPill
  }
}
