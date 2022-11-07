package adventofcode.Day3

import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

def computeFrequencies[A](lst: List[A]): Map[A, Int] =
  lst.groupBy(identity).view.mapValues(_.length).toMap

def mostCommon(lst: List[Char]): Char =
  val frequencies = computeFrequencies(lst)
  if frequencies('0') > frequencies('1') then '0' else '1'

def leastCommon(lst: List[Char]): Char =
  val frequencies = computeFrequencies(lst)
  if frequencies('1') < frequencies('0') then '1' else '0'

def singlePassRateCalculator(input: List[List[Char]], compF: List[Char] => Char): Int =
  val binaryString = input.transpose.map(compF).mkString
  Integer.parseInt(binaryString, 2)

def multiPassRateCalculator(input: List[List[Char]], compF: List[Char] => Char): Int =
  @tailrec
  def loop(candidates: List[List[Char]], index: Int): String =
    if candidates.length == 1 then
      return candidates.head.mkString
    val columns = candidates.transpose
    val currentColumn = columns(index)
    val target = compF(currentColumn)
    loop(candidates.filter(_(index) == target), index + 1)

  Integer.parseInt(loop(input, 0), 2)


@main def day03(): Unit =
  val input = Using.resource(Source.fromFile("input/day03.txt"))(_.getLines.toList)
  val parsed = input.map(string => string.map(c => c).toList)

  val gammaRate = singlePassRateCalculator(parsed, mostCommon)
  val epsilonRate = singlePassRateCalculator(parsed, leastCommon)
  val part1 = gammaRate * epsilonRate

  val oxygenGeneratorRating = multiPassRateCalculator(parsed, mostCommon)
  val CO2ScrubberRating = multiPassRateCalculator(parsed, leastCommon)
  val part2 = oxygenGeneratorRating * CO2ScrubberRating

  println(part1)
  println(part2)
