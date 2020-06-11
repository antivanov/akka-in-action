package aia.state

import akka.agent.Agent
import akka.actor.ActorSystem

import concurrent.{Await, Future}
import concurrent.duration._
import akka.util.Timeout


case class BookStatistics(book: String, nrSold: Int)
case class StateBookStatistics(sequence: Long, books: Map[String, BookStatistics])


class BookStatisticsMgr(system: ActorSystem) {
  implicit val ex = system.dispatcher //todo: change chapter 2.2 =>2.3
  val stateAgent = Agent(StateBookStatistics(0, Map())) //todo: change chapter 2.2 =>2.3

  private def updateBooksSold(book: String, nrSold: Int)(oldState: StateBookStatistics): StateBookStatistics = {
    val bookStat = oldState.books.get(book) match {
      case Some(bookState) =>
        bookState.copy(nrSold = bookState.nrSold + nrSold)
      case None => BookStatistics(book, nrSold)
    }
    oldState.copy(oldState.sequence + 1,
      oldState.books + (book -> bookStat))
  }

  def addBooksSold(book: String, nrSold: Int): Unit =
    stateAgent send (updateBooksSold(book, nrSold) _)

  def addBooksSoldAndReturnNewState(book: String,
                                    nrSold: Int): Future[StateBookStatistics] =
    stateAgent alter (updateBooksSold(book, nrSold) _)

  def getStateBookStatistics(): StateBookStatistics =
    stateAgent.get()
}
