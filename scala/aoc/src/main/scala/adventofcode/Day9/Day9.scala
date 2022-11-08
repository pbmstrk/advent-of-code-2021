package adventofcode.Day9

import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

type Point = (Int, Int)

def parseList(gridlist: List[List[Char]]): Map[Point, Int] =
  val points = for (row, r) <- gridlist.zipWithIndex
                   (value, c) <- row.zipWithIndex
    yield ((r, c), Character.digit(value, 10))
  points.toMap

class Grid(gridList: List[List[Char]]):
  val gridMap: Map[Point, Int] = parseList(gridList)

  def neighbours(p: Point): List[Point] =
    val (x, y)=  p
    List((x+1, y), (x-1,y), (x, y+1), (x, y-1)).filter(x => gridMap.contains(x))

  def isLowPoint(p: Point): Boolean =
    neighbours(p).map(x => gridMap(x)).forall(v => v > gridMap(p))

  def lowPoints: List[Point] =
    gridMap.keys.toList.filter(isLowPoint)

  def riskLevel(p: Point): Int =
    gridMap(p) + 1

  def expandBasin(p: Point): List[Point] =
    @tailrec
    def loop(seen: Set[Point], todo: Set[Point]): List[Point] =
      if todo.isEmpty then
        seen.toList
      else
        val newSeen = seen ++ todo
        val neighs = todo.flatMap(neighbours)
        val newTodo = neighs.filter(p => (gridMap(p) < 9) && (!seen.contains(p)))
        loop(newSeen, newTodo)
    loop(Set.empty[Point], Set(p))

@main def day9(): Unit =
  val input = Using.resource(Source.fromFile("input/day09.txt"))(_.getLines.map(x => x.toList).toList)
  val grid = Grid(input)
  val lowPoints = grid.lowPoints

  val part1 = lowPoints.map(grid.riskLevel).sum
  val part2 = lowPoints.map(p => grid.expandBasin(p).length).sorted.reverse.take(3).product
  println(part1)
  println(part2)