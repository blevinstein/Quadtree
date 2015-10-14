/**
 * Copied from
 * http://doc.akka.io/docs/akka/2.0/intro/getting-started-first-scala.html
 * Copyright (C) 2009-2012 Typesafe Inc. <http://www.typesafe.com>
 *
 * Minor modifications.
 */

package com.blevinstein.learn_akka

import akka.actor._
import akka.routing.RoundRobinPool

import scala.concurrent.duration._

object Pi extends App {

  calculate(nrOfWorkers = 4, nrOfElements = 10000, nrOfMessages = 10000)

  sealed trait PiMessage
  case object Calculate extends PiMessage
  case class Work(start: Int, nrOfElements: Int) extends PiMessage
  case class Result(value: Double) extends PiMessage
  case class PiApproximation(pi: Double, duration: Duration)

  class Worker extends Actor {
    def calculatePiFor(start: Int, nrOfElements: Int): Double = {
      var acc = 0.0
      for (i <- start until (start + nrOfElements))
        acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
      acc
    }

    def receive = {
      case Work(start, nrOfElements) =>
          sender ! Result(calculatePiFor(start, nrOfElements))
    }
  }

  class Master(nrOfWorkers: Int,
      nrOfMessages: Int,
      nrOfElements: Int,
      listener: ActorRef) extends Actor {

    var pi: Double = _
    var nrOfResults: Int = _
    val start: Long = System.currentTimeMillis

    val workerRouter = context.actorOf(
        RoundRobinPool(nrOfWorkers).props(Props[Worker]), "workerRouter")

    def receive = {
      case Calculate =>
        for (i <- 0 until nrOfMessages)
          workerRouter ! Work(i * nrOfElements, nrOfElements)
      case Result(value) =>
        pi += value
        nrOfResults += 1
        if (nrOfResults == nrOfMessages) {
          val end = System.currentTimeMillis
          listener ! PiApproximation(pi, duration = (end - start).millis)
          context.stop(self)
        }
    }
  }

  class Listener extends Actor {
    def receive = {
      case PiApproximation(pi, duration) =>
        println(s"Pi = $pi\nDuration: $duration")
        context.system.shutdown()
    }
  }

  def calculate(nrOfWorkers: Int, nrOfElements: Int, nrOfMessages: Int) {
    val system = ActorSystem("PiSystem")

    val listener = system.actorOf(Props[Listener], name = "listener")

    val master = system.actorOf(Props(new Master(
      nrOfWorkers, nrOfMessages, nrOfElements, listener)),
      name = "master")

    master ! Calculate
  }
}
