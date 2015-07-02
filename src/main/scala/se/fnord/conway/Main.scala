package se.fnord.conway

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import se.fnord.conway.Engine.Board

case class Evolve()
case class Render(board : Board)

object Main {
  val initialBoard = Matrix(Array[Seq[Cell]](
    Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, LiveCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, LiveCell, LiveCell, LiveCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, LiveCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell),
    Array(DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell, DeadCell)
  ), DeadCell)


  def main(args: Array[String]) {
    val system = ActorSystem("conway")

    val renderer = system.actorOf(Props[Renderer], "renderer")
    system.eventStream.subscribe(renderer, classOf[Render])

    val evolver = system.actorOf(Props(classOf[Evolver], initialBoard), "evolver")

    system.scheduler.schedule(1.second, 1.second, evolver, Evolve())
  }
}

class Evolver(var board : Board) extends Actor with EventStreamPublisher {
  val once = Once()
  def receive = {
    case Evolve() => {
      once {
        publish(Render(board))
      }
      val newBoard = Engine.evolve(board)
      board = newBoard
      publish(Render(newBoard))
    }
  }
}

class Renderer extends Actor with EventStreamSubscriber {
  override def preStart() = {
    subscribe(classOf[Render])
  }
  override def postStop() = {
    unsubscribe(classOf[Render])
  }
  def receive = {
    case Render(board) => {
      println(board)
      println()
    }
  }
}