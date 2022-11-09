package adventofcode.Day13

import scala.io.Source
import scala.util.Using

type Point = (Int, Int)

enum Instruction:
  case FoldX(x: Int)
  case FoldY(y: Int)

extension (s: String)
  def splitLines = s.split("\\n")
  def splitBlankLines = s.split("\\n\\n")

def parsePoint(s: String): Point =
  val ints = s.split(',').map(s => Integer.parseInt(s))
  ints match
    case Array(c, r) => (r, c)

def parseInstruction(s: String): Instruction =
  s match
    case s"fold along x=$v" => Instruction.FoldX(v.toInt)
    case s"fold along y=$v" => Instruction.FoldY(v.toInt)

def parse(input: String): (Array[Point], Array[Instruction]) =
  input.splitBlankLines match
    case Array(points, instructions) => (
      points.splitLines.map(parsePoint),
      instructions.splitLines.map(parseInstruction))

def foldPaper(points: Array[Point], instruction: Instruction): Array[Point] =
  instruction match
    case Instruction.FoldX(column) =>
      points.collect { case (r,c) if c != column => if c < column then (r,c) else (r, column - (c - column)) }
    case Instruction.FoldY(row) =>
      points.collect { case (r, c) if r != row => if r < row then (r,c) else (row - (r - row), c)}

def formatPoints(ps : Array[Point]): Vector[Vector[Char]] =
  val (minR, maxR) = (ps.map(p => p._1).min , ps.map(p => p._1).max)
  val (minC, maxC) = (ps.map(p => p._2).min , ps.map(p => p._2).max)
  Vector.range(minR, maxR+1).map(r => Vector.range(minC, maxC+1)
    .map(c => if ps.contains((r,c)) then '#' else ' '))

@main def day13(): Unit =
  val input = Using.resource(Source.fromFile("input/day13.txt"))(_.mkString)
  val (points, instructions) = parse(input)

  val part1 = foldPaper(points, instructions.head).distinct.length
  val finalPoints = instructions.foldLeft(points)(foldPaper)
  println(part1)
  for r <- formatPoints(finalPoints) do
    println(r.mkString(" "))