package aia.state

import akka.testkit.{TestKit, TestProbe}
import akka.actor.ActorSystem
import org.scalatest.{BeforeAndAfterAll, MustMatchers, WordSpecLike}
import akka.agent.Agent

import scala.concurrent.stm._
import scala.concurrent.duration._
import concurrent.{Await, Future}
import akka.util.Timeout
import org.scalatest.concurrent.ScalaFutures

class AgentTest extends TestKit(ActorSystem("AgentTest"))
  with WordSpecLike with BeforeAndAfterAll with MustMatchers with ScalaFutures {
  implicit val ec = system.dispatcher
  implicit val timeout = Timeout(3 seconds)
  override def afterAll(): Unit = {
    system.terminate()
  }

  "Agent" must {
    "test1" in {
      val agent = Agent(StateBookStatistics(0, Map()))

      Future {
        Thread.sleep(200)
        agent send (new StateBookStatistics(22, Map()))
      }

      agent() mustEqual StateBookStatistics(0, Map())

      Thread.sleep(300)
      val state = agent()
      state mustEqual StateBookStatistics(22, Map())

      agent send StateBookStatistics(state.sequence + 1, Map())
      agent.future().futureValue mustEqual StateBookStatistics(23, Map())
    }
    "test2" in {
      val agent = Agent(new StateBookStatistics(0, Map()))

      Future {
        Thread.sleep(200)
        agent send (new StateBookStatistics(22, Map()))
      }
      agent() mustEqual StateBookStatistics(0, Map())
      Thread.sleep(300)
      agent() mustEqual StateBookStatistics(22, Map())

      agent.send(oldState => oldState.copy(sequence = oldState.sequence + 1))
      agent.future().futureValue mustEqual StateBookStatistics(23, Map())
    }
    "test3" in {
      val agent = Agent(StateBookStatistics(0, Map()))
      val func = (oldState: StateBookStatistics) => {
        oldState.copy(sequence = oldState.sequence + 1)
      }
      agent.send(func)

      agent() mustEqual StateBookStatistics(0, Map())
      agent.future().futureValue mustEqual StateBookStatistics(1, Map())
    }
    "test4" in {
      val probe = TestProbe()
      val agent = Agent(StateBookStatistics(0, Map()))
      val func = (oldState: StateBookStatistics) => {
        Thread.sleep(100)
        if (oldState.sequence == 0)
          probe.ref ! "test"
        oldState.copy(sequence = oldState.sequence + 1)
      }
      agent.send(func)
      agent() mustEqual StateBookStatistics(0, Map())
      probe.expectMsg("test")
      agent.future().futureValue mustEqual StateBookStatistics(1, Map())

    }
    "test5" in {
      val agent1 = Agent(3)
      val agent4 = agent1 map (_ + 1)

      agent4.future().futureValue mustEqual 4
      agent1 send (_ + 2)
      agent1.future().futureValue mustEqual 5
      agent4.future().futureValue mustEqual 4
    }
  }
  "AgentMgr" must {
    "test" in {
      val bookName = "Akka in Action"
      val mgr = new BookStatisticsMgr(system)
      mgr.addBooksSold(bookName, 1)
      mgr.addBooksSold(bookName, 1)

      mgr.stateAgent.future().futureValue

      mgr.getStateBookStatistics() mustEqual StateBookStatistics(2, Map(bookName -> BookStatistics(bookName, 2)))
    }
    "test alter" in {
      val bookName = "Akka in Action"
      val mgr = new BookStatisticsMgr(system)
      mgr.addBooksSold(bookName, 1)

      val state = mgr.addBooksSoldAndReturnNewState(bookName, 1).futureValue

      state mustEqual StateBookStatistics(2, Map(bookName -> BookStatistics(bookName, 2)))
    }
  }
}
