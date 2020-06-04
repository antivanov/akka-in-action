package aia.testdriven

import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import akka.testkit.{EventFilter, TestKit}
import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.concurrent.ScalaFutures

import concurrent.duration._

class GreetingsSpec extends TestKit(GreetingsSpec.testSystem) with WordSpecLike with MustMatchers with ScalaFutures with BeforeAndAfterAll {

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = 5 seconds, interval = 100 milliseconds)
  implicit val timeout = Timeout(1 second)
  "A greeting actor " must {
    "respond to a greeting and tell name" in {
      val greetingActorRef = system.actorOf(Props(new GreetingActor(name = "Alice")))
      val response = greetingActorRef.ask(Greeting("Bob"))
      response.mapTo[Greeting].futureValue mustEqual Greeting("Alice", "Hello Bob")
    }
    "be able to receive a greeting and log it, version 1" in {
      val greetingActorRef = system.actorOf(Props[GreetingActor1])
      EventFilter.info(message = "Hello Bob",
        occurrences = 1).intercept {
        greetingActorRef ! Greeting("Bob")
      }
    }

    "be able to receive a greeting and log it, version 1 with WireTap" in {
      val greetingActorRef = system.actorOf(Props(new GreetingActor1 with WireTap))

      EventFilter.info(message = "Hello Bob",
        occurrences = 1).intercept {
        greetingActorRef ! Greeting("Bob")
        expectMsg(Greeting("Bob"))
      }
    }

    "respond with a greeting and log it, version 2" in {
      val greetingActorRef = system.actorOf(Props(new GreetingActor2(testActor)))
      EventFilter.info(message = "Hello Bob",
        occurrences = 1).intercept {
        greetingActorRef ! Greeting("Bob")
        expectMsg("Hello Bob")
      }
    }

    "aggregate messages and return them when requested, version 3" in {
      val greetingActorRef = system.actorOf(Props(new GreetingActor3()))
      val messagesNumber = 10
      val messages = (0 to messagesNumber).map({ idx =>
        Greeting(f"Bob${idx}")
      })
      messages.foreach { message =>
        greetingActorRef ! message
      }
      greetingActorRef.ask(GetGreetings).mapTo[Vector[Greeting]].futureValue mustEqual messages
    }
  }

  override protected def afterAll(): Unit = {
    system.terminate()
  }

  trait WireTap extends Actor {
    abstract override def receive = {
      case m =>
        super.receive(m)
        testActor ! m
    }
  }

}

case class Greeting(name: String, greeting: String = "Hello")

case class GetGreetings()

class GreetingActor(val name: String) extends Actor {
  def receive = {
    case msg: Greeting => sender() ! Greeting(name, s"Hello ${msg.name}")
  }
}


class GreetingActor1 extends Actor with ActorLogging {
  def receive = {
    case msg: Greeting => log.info(s"Hello ${msg.name}")
  }
}

class GreetingActor2(listener: ActorRef) extends Actor with ActorLogging {
  def receive = {
    case msg: Greeting =>
      val printMessage = s"Hello ${msg.name}"
      log.info(printMessage)
      listener ! printMessage
  }
}

class GreetingActor3 extends Actor with ActorLogging {
  private var receivedGreetings = Vector[Greeting]()

  def receive = {
    case msg: Greeting =>
      log.info("Hello %s" format msg.name)
      receivedGreetings = receivedGreetings :+ msg
    case msg @ GetGreetings =>
      sender() ! receivedGreetings
  }
}

object GreetingsSpec {
  val testSystem = {
    val config: Config = ConfigFactory.parseString(
      """
         akka.loggers = [akka.testkit.TestEventListener]
      """)
    ActorSystem("testsystem", config)
  }
}

