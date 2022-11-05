package adventofcode.Day10

import scala.io.Source
import scala.util.Using

val matching: Map[Char, Char] = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

def isClosingChar(c: Char): Boolean =
  matching.contains(c)

enum Result:
  case FirstCorruptedChar(c: Char)
  case ClosingChars(c: List[Char])

def reduceCharList(lst: List[Char]): Result =
  def loop(charList: List[Char], stack: List[Char]): Result =
    charList match
      case Nil => Result.ClosingChars(stack)
      case c :: cs => {
        if !(isClosingChar(c)) then
          loop(cs, c :: stack)
        else
          stack match
            case Nil => if isClosingChar(c) then Result.FirstCorruptedChar(c) else loop(cs, List(c))
            case s :: ss => if matching(c) == s then loop(cs, ss) else Result.FirstCorruptedChar(c)
        }
  loop(lst, List.empty[Char])

def charScore(c: Char): Int =
  c match
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137

def scoreRemaining(cs: List[Char]): BigInt =
  def loop(remaining: List[Char], acc: BigInt): BigInt =
    remaining match
      case Nil => acc
      case c :: rest => c match
        case '(' => loop(rest, 5*acc + 1)
        case '[' => loop(rest, 5*acc + 2)
        case '{' => loop(rest, 5*acc + 3)
        case '<' => loop(rest, 5*acc + 4)
  loop(cs, 0)

extension [T](xs: List[T])
  def middleElement = xs(xs.length / 2)

@main def day10(): Unit =
  val input = Using.resource(Source.fromFile("input/day10.txt"))(_.getLines.toList)
  val charLists = input.map(x => x.toList)
  var resultList = charLists.map(reduceCharList)

  val part1  = resultList.collect {case Result.FirstCorruptedChar(c) => c }.map(charScore).sum
  val part2 = resultList.collect {case Result.ClosingChars(cs) => cs}.map(x => scoreRemaining(x)).sorted.middleElement

  println(part1)
  println(part2)