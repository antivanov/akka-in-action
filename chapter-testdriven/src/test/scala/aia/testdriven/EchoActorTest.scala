package aia.testdriven

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Actor, ActorSystem, Props}
import org.scalatest.{Matchers, WordSpecLike}
import akka.util.Timeout

import scala.concurrent.Await
import scala.util.{Failure, Success}
import scala.language.postfixOps


class EchoActorTest extends TestKit(ActorSystem("testsystem"))
  with WordSpecLike
  with ImplicitSender
  with StopSystemAfterAll
  with Matchers {


  "An EchoActor" must {
    val message = "some message"

    "Reply with the same message it receives" in {

      import akka.pattern.ask
      import scala.concurrent.duration._
      implicit val timeout = Timeout(3 seconds)
      implicit val ec = system.dispatcher
      val echo = system.actorOf(Props(new EchoActor()), "echo1")
      val future = echo.ask(message)

      future.onComplete {
        case Failure(e)   =>
          fail("Did not successfully receive message back", e)
        case Success(msg) =>
          message shouldEqual msg
      }

      Await.ready(future, timeout.duration)
    }

    "Reply with the same message it receives without ask" in {
      val echo = system.actorOf(Props[EchoActor], "echo2")
      echo ! message
      expectMsg(message)

    }

  }
}


class EchoActor extends Actor {
  def receive = {
    case msg =>
      sender() ! msg
  }
}

