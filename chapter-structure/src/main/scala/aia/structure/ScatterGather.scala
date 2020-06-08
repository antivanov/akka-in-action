package aia.structure

import java.util.Date
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer

import akka.actor._
import java.text.SimpleDateFormat

case class PhotoMessage(id: String,
                        photo: PhotoString,
                        creationTime: Option[Date] = None,
                        speed: Option[Int] = None)

case class PhotoString(value: String) extends AnyVal

object PhotoString {
  val dateFormat = new SimpleDateFormat("ddMMyyyy HH:mm:ss.SSS")
  def getSpeed(photoString: PhotoString): Option[Int] = {
    val attributes = photoString.value.split('|')
    if (attributes.size == 3)
      Some(attributes(1).toInt)
    else
      None
  }
  def getTime(photoString: PhotoString): Option[Date] = {
    val attributes = photoString.value.split('|')
    if (attributes.size == 3)
      Some(dateFormat.parse(attributes(0)))
    else
      None
  }
  def getLicense(photoString: PhotoString): Option[String] = {
    val attributes = photoString.value.split('|')
    if (attributes.size == 3)
      Some(attributes(2))
    else
      None
  }

  def createPhotoString(date: Date,
                        speed: Int,
                        license: Option[String] = None): PhotoString =
    PhotoString("%s|%s|%s".format(dateFormat.format(date), speed, license.getOrElse(" ")))
}

class GetSpeed(pipe: ActorRef) extends Actor {
  def receive = {
    case msg: PhotoMessage => {
      pipe ! msg.copy(
        speed = PhotoString.getSpeed(msg.photo))
    }
  }
}
class GetTime(pipe: ActorRef) extends Actor {
  def receive = {
    case msg: PhotoMessage => {
      pipe ! msg.copy(creationTime =
        PhotoString.getTime(msg.photo))
    }
  }
}



class RecipientList(recipientList: Seq[ActorRef]) extends Actor {
  def receive = {
    case msg: AnyRef => recipientList.foreach(_ ! msg)
  }
}


case class TimeoutMessage(msg: PhotoMessage)


class Aggregator(timeout: FiniteDuration, pipe: ActorRef)
  extends Actor {

  val messages = new ListBuffer[PhotoMessage]
  implicit val ec = context.system.dispatcher
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    messages.foreach(self ! _)
    messages.clear()
  }

  def receive = {
    case rcvMsg: PhotoMessage => {
      messages.find(_.id == rcvMsg.id) match {
        case Some(alreadyRcvMsg) => {
          val newCombinedMsg = new PhotoMessage(
            rcvMsg.id,
            rcvMsg.photo,
            rcvMsg.creationTime.orElse(alreadyRcvMsg.creationTime),
            rcvMsg.speed.orElse(alreadyRcvMsg.speed))
          pipe ! newCombinedMsg
          //cleanup message
          messages -= alreadyRcvMsg
        }
        case None => {
          messages += rcvMsg
          context.system.scheduler.scheduleOnce(
            timeout,
            self,
            new TimeoutMessage(rcvMsg))
        }
      }
    }
    case TimeoutMessage(rcvMsg) => {
      messages.find(_.id == rcvMsg.id) match {
        case Some(alreadyRcvMsg) => {
          pipe ! alreadyRcvMsg
          messages -= alreadyRcvMsg
        }
        case None => //message is already processed
      }
    }
    case ex: Exception => throw ex
  }
}
