package adventofcode.Day7

import scala.io.Source
import scala.util.Using

def computeDistancesFromPointWith(target: Int, points: List[Int], f: Int => Int): List[Int]  =
  points.map(x => f(Math.abs(target - x)))

def findMinimumWith(points: List[Int], f: Int => Int): Int =
  val minValue = points.min
  val maxValue = points.max
  Range(minValue, maxValue).map(x => computeDistancesFromPointWith(x, points, f).sum).min

def sumUpToN(n: Int): Int = (n * (n + 1))/ 2

@main def day7(): Unit =
  val points = Using.resource(Source.fromFile("input/day07.txt"))(_.mkString.split(',').map(_.toInt).toList)

  val part1 = findMinimumWith(points, identity)
  val part2 = findMinimumWith(points, sumUpToN)

  println(part1)
  println(part2)