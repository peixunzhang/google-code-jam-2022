package example

import scala.io.StdIn
import scala.collection.mutable
import java.util.Comparator

object excercise2 extends App {
  sealed trait Event {
    def time: Long
    def sortValue: Int
  }

  object Event {
    final case class Delivery(time: Long, numberOfLeaves: Int, validFor: Long) extends Event {
      val sortValue = 1
    }
    final case class Expire(time: Long, numberOfLeaves: Int) extends Event {
      val sortValue = 0
    }
    final case class Order(time: Long, leavesRequired: Int) extends Event {
      val sortValue = 2
    }

    implicit val ordering: Ordering[Event] =
      Ordering.by(e => (e.time, e.sortValue))
  }

  final case class TestCase(number: Int, events: List[Event]) {
    def result = {
      val queue = mutable.PriorityQueue.empty[Event].reverse
      events.foreach { event =>
        queue.enqueue(event)  
      }

      var completed = 0
      var leaves = mutable.Map.empty[Long, Int].withDefaultValue(0)
      var continue = true

      def takeLeaves(times: List[Long], numberOfLeaves: Int): Boolean =
        times match {
          case Nil => false
          case time :: rest =>
            val leavesInTime = leaves(time)
            if (leavesInTime >= numberOfLeaves) {
              leaves.update(time, leaves(time) - numberOfLeaves)
              true
            } else {
              leaves.remove(time)
              takeLeaves(rest, numberOfLeaves - leavesInTime)
            }
        }

      while (continue && queue.nonEmpty) {
        queue.dequeue() match {
          case Event.Delivery(time, numberOfLeaves, validFor) =>
            leaves.update(time + validFor, leaves(time + validFor) + numberOfLeaves)
            queue.enqueue(Event.Expire(time + validFor, numberOfLeaves))
          case Event.Expire(time, numberOfLeaves) =>
            leaves.update(time, Math.max(0, leaves(time) - numberOfLeaves))
          case Event.Order(time, leavesRequired) =>
            continue = takeLeaves(leaves.keys.toList.sorted, leavesRequired)
            if (continue) {
              completed += 1
            }
        }
      }

      completed
    }

    def run() =
      println(s"Case #$number: $result")
  }

  def readInput() = {
    val result = mutable.ListBuffer.empty[String]
    var line = StdIn.readLine()
    while (line != null) {
      result += line
      line = StdIn.readLine()
    }
    result.toList
  }

  def parse(input: List[String]) = {
    val numberOfCases = input.head.toInt
    def go(input: List[String], n: Int, acc: List[TestCase] = Nil): List[TestCase] = {
      if (n <= 0) acc.reverse
      else {
        val List(nDeliviers, _, leavesPerOrder) = input.head.split(" ").toList.map(_.toInt)
        val deliviers = input.tail.take(nDeliviers).map { line =>
          val List(time, leaves, validFor) = line.split(" ").toList
          Event.Delivery(time.toLong, leaves.toInt, validFor.toLong)  
        }
        val orders = input.tail.drop(nDeliviers).head.split(" ").map(time => Event.Order(time.toLong, leavesPerOrder))
        val testCase = TestCase(
          acc.size + 1,
          deliviers ++ orders
        )
        go(input.drop(nDeliviers + 2), n - 1, testCase :: acc)
      }
    }
    go(input.tail, numberOfCases)
  }

  parse(readInput()).foreach(_.run())
}
