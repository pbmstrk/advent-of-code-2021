package adventofcode.Day2

import scala.io.Source
import scala.util.Using

enum Command:
  case Forward(x: Int)
  case Down(x: Int)
  case Up(x: Int)

trait BasePosition:
  def result: Int
case class Position(position: Int, depth: Int) extends BasePosition:
  override def result: Int = position * depth
case class PositionWithAim(position: Int, depth: Int, aim: Int) extends BasePosition:
  override def result: Int = position * depth

def parseCommand(input: String): Command =
  input match
    case s"forward $x" => Command.Forward(x.toInt)
    case s"up $x" => Command.Up(x.toInt)
    case s"down $x" => Command.Down(x.toInt)

def updatePosition(p: Position, c: Command): Position =
    c match
      case Command.Forward(x) => p.copy(position = p.position + x)
      case Command.Down(x) => p.copy(depth = p.depth + x)
      case Command.Up(x) => p.copy(depth = p.depth - x)

def updatePositionWithAim(p: PositionWithAim, c: Command): PositionWithAim =
  c match
    case Command.Forward(x) => p.copy(position = p.position + x, depth= p.depth + p.aim*x)
    case Command.Down(x) => p.copy(aim= p.aim + x)
    case Command.Up(x) => p.copy(aim = p.aim - x)

@main def day02(): Unit =
  val input = Using.resource(Source.fromFile("input/day02.txt"))(_.getLines.toList)
  val commands = input.map(parseCommand)

  val part1 = commands.foldLeft(Position(0,0))(updatePosition).result
  val part2 = commands.foldLeft(PositionWithAim(0,0,0))(updatePositionWithAim).result
  println(part1)
  println(part2)
