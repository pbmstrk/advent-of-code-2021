package adventofcode.Day20

import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

type Point = (Int, Int)
type Image = Vector[Vector[Char]]
type Algorithm = Vector[Char]

def parsePixel(c: Char): Char =
  c match
    case '.' => '0'
    case '#' => '1'

def parse(input: String): (Algorithm, Image) =
  input.split("""\n\n""") match
    case Array(alg, img) => (
      alg.toVector.map(parsePixel),
      img.split("""\n""").toVector.map(line => line.toVector.map(parsePixel)))

def getWindow(p: Point): List[Point] =
  val (r, c) = p
  List((r-1, c-1), (r-1,c), (r-1,c+1), (r, c-1), (r,c), (r, c+1), (r+1, c-1), (r+1, c), (r+1, c+1))

def getValue(image: Image, p: Point, default: Char): Char =
  val (r,c) = p
  image.lift(r).flatMap(_.lift(c)).getOrElse(default)

def computeNewPixel(p: Point, image: Image, alg: Algorithm, default: Char): Char =
  val windowValues = getWindow(p).map(x => getValue(image, x, default))
  val algorithmIndex = Integer.parseInt(windowValues.mkString, 2)
  alg(algorithmIndex)

def nextRound(image: Image, algorithm: Algorithm, default: Char): Image =
  val numRows = image.length
  val numCols = image(0).length
  Vector.range(-1, numRows + 1)
    .map(r => Vector.range(-1, numCols + 1)
      .map(c => computeNewPixel((r,c), image, algorithm, default)))

def getDefaultValue(iteration: Int): Char =
  if iteration % 2 == 0 then '0' else '1'

def simulate(image: Image, algorithm: Algorithm, steps: Int): Image =
  @tailrec
  def loop(im: Image, n: Int): Image =
    if n == steps then im
    else loop(nextRound(im, algorithm, getDefaultValue(n)), n+1)
  loop(image, 0)

def countPixels(image: Image): Int =
  image.flatten.count(_ == '1')

@main def day20(): Unit =
  val input = Using.resource(Source.fromFile("input/day20.txt"))(_.mkString)
  val (algorithm, image) = parse(input)

  val part1 = countPixels(simulate(image, algorithm,2))
  val part2 = countPixels(simulate(image, algorithm,50))
  println(part1)
  println(part2)

