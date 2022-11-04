package adventofcode.Day6

import scala.io.Source
import scala.util.Using

def updateMap(m: Map[Int, BigInt], key: Int, update: BigInt): Map[Int, BigInt] =
  val existingValue: BigInt = m.getOrElse(key, 0)
  m + (key -> (existingValue + update))

def updateTimer(timer: Int): Int =
  if timer == 0 then 6 else timer - 1

def tick(freqMap: Map[Int, BigInt]): Map[Int, BigInt] =
  val day0Fish: BigInt = freqMap.getOrElse(0, 0)
  val r = freqMap.foldLeft(Map.empty[Int, BigInt]){case (m, (k, v)) => updateMap(m, updateTimer(k), v)}
  r.updated(8, day0Fish)

def simulate(freqMap: Map[Int, BigInt], days: Int): BigInt =
  val finalCount = (1 to days).foldLeft(freqMap)((m, _) => tick(m))
  finalCount.values.sum

@main def day06(): Unit =
  val input = Using.resource(Source.fromFile("input/day06.txt"))(_.mkString)
  val lanternFish = input.split(',').map(_.toInt)
  val initialFrequencyMap: Map[Int, BigInt] = lanternFish.groupMapReduce(identity)(_ => BigInt(1))(_ + _)

  val part1 = simulate(initialFrequencyMap, 80)
  val part2 = simulate(initialFrequencyMap, 256)

  println(part1)
  println(part2)
