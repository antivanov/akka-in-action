package aia.testdriven


import akka.actor.{ActorLogging, Actor}

case class GreetingMessage(message: String)

class Greeter extends Actor with ActorLogging {
  def receive = {
    case GreetingMessage(message) => log.info("Hello {}!", message)
  }
}

