package adventofcode.Day5

import scala.io.Source
import scala.util.Using

case class Point(x: Int, y: Int)

def parseLineEndPoints(line: String): (Point, Point) =
  line match
    case s"${x1},${y1} -> ${x2},${y2}" => (Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    case _ => throw new Exception

def newRange(x: Int, y: Int): scala.collection.immutable.Range =
  if x <= y then Range(x, y+1) else Range(x, y-1, -1)

def isStraightLine(pair: (Point, Point)): Boolean =
  val (p1, p2) = pair
  if (p1.x == p2.x) || (p1.y == p2.y) then true else false

def getPoints(pair: (Point, Point)): IndexedSeq[Point] =
  val (p1, p2) = pair
  if (p1.x == p2.x) then
    for y <- newRange(p1.y, p2.y) yield Point(p1.x, y)
  else if (p1.y == p2.y) then
    for x <- newRange(p1.x, p2.x) yield Point(x, p1.y)
  else for (a, b) <- newRange(p1.x, p2.x).zip(newRange(p1.y, p2.y)) yield Point(a, b)

extension [T](xs: List[T])
  def frequencies = xs.groupBy(identity).view.mapValues(_.length).toMap

def countOverlaps(lines: List[(Point, Point)]): Int =
  lines.flatMap(getPoints).frequencies.filter((_, c) => c > 1).size

@main def day05(): Unit =
  val input = Using.resource(Source.fromFile("input/day05.txt"))(_.getLines.toList)
  val lines = input.map(parseLineEndPoints)
  
  val part1 = countOverlaps(lines.filter(isStraightLine))
  val part2 = countOverlaps(lines)
  println(part1)
  println(part2)