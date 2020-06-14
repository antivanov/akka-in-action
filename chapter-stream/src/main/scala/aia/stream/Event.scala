package aia.stream

import java.time.ZonedDateTime

case class Event(
  host: String,
  service: String,
  state: State,
  time: ZonedDateTime,
  description: String,
  tag: Option[String] = None, 
  metric: Option[Double] = None
)


sealed trait State
case object Critical extends State
case object Error  extends State
case object Ok extends State
case object Warning extends State

object State {
  def norm(str: String): String = str.toLowerCase
  def norm(state: State): String = norm(state.toString)

  val values: List[State] = List(Critical, Error, Ok, Warning)

  private def valueMapping: Map[String, State] = values.foldLeft(Map[String, State]()) { (acc, state) =>
    acc + (norm(state) -> state)
  }

  def unapply(str: String): Option[State] =
    valueMapping.get(norm(str))
}

case class LogReceipt(logId: String, written: Long)
case class ParseError(logId: String, msg: String)