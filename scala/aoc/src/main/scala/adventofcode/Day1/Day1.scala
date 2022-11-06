package adventofcode.Day1

import scala.io.Source
import scala.util.Using

def countIncreases(lst: List[Int], n: Int): Int =
  lst.zip(lst.drop(n)).count((a, b) => b > a)

@main def day01(): Unit =
  val input = Using.resource(Source.fromFile("input/day01.txt"))(_.getLines.toList)
  val measurements = input.map(_.toInt)

  val part1 = countIncreases(measurements, 1)
  val part2 = countIncreases(measurements, 3)
  println(part1)
  println(part2)

