package example

import scala.io.StdIn
import scala.collection.mutable.ListBuffer

object excercise1 extends App {
  final case class TestCase(number: Int, lu: Int, ld: Int, ru: Int, rd: Int) {
    def numberFlip: Int = 
      Math.abs(lu - rd) + Math.abs(ld - ru)

    def run() = 
      println(s"Case #$number: $numberFlip")
  }

  def readInput() = {
    val result = ListBuffer.empty[String]
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
        val dims = input.head.toInt
        val (data, rest) = input.tail.splitAt(dims * 2)
        val testCase = TestCase(
          acc.size + 1,
          lu = data.take(dims).map(_.take(dims)).foldLeft(0) { case (acc, next) => acc + next.count(_ == 'I')},
          ld = data.drop(dims).map(_.take(dims)).foldLeft(0) { case (acc, next) => acc + next.count(_ == 'I')},
          ru = data.take(dims).map(_.drop(dims)).foldLeft(0) { case (acc, next) => acc + next.count(_ == 'I')},
          rd = data.drop(dims).map(_.drop(dims)).foldLeft(0) { case (acc, next) => acc + next.count(_ == 'I')}
        )

        go(rest, n - 1, testCase :: acc)
      }
    }
    go(input.tail, numberOfCases)
  }

  parse(readInput()).foreach(_.run())
}
