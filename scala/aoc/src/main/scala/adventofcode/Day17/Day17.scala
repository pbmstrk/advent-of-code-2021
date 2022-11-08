package adventofcode.Day17

import scala.annotation.tailrec

case class Target(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

case class State(xPos: Int, yPos: Int, xVelocity: Int, yVelocity: Int):
  def hitTarget(target: Target): Boolean =
    xPos >= target.xMin && xPos <= target.xMax && yPos >= target.yMin && yPos <= target.yMax

  def overshot(target: Target): Boolean =
    xPos > target.xMax || yPos < target.yMin

  def update: State =
    val updatedVelocity =
      if xVelocity < 0 then xVelocity + 1
      else if xVelocity > 0 then xVelocity - 1
      else 0
    State(xPos + xVelocity, yPos + yVelocity, updatedVelocity, yVelocity - 1)

def runSimulation(initialState: State, target: Target): Option[Int] =
  @tailrec
  def loop(state: State, yMax: Int, n: Int): Option[Int] =
    if n > 1000 then None
    else if state.overshot(target) then None
    else if state.hitTarget(target) then Some(yMax)
    else
      val newState = state.update
      loop(newState, scala.math.max(yMax, newState.yPos), n + 1)

  loop(initialState, Integer.MIN_VALUE, 0)


@main def day17(): Unit =
  val target = Target(85, 145, -163, -108)
  val heightsOption = for xV <- Range(0, 200)
                          yV <- Range(-200, 200)
  yield runSimulation(State(0, 0, xV, yV), target)
  val heights = heightsOption.flatten

  val part1 = heights.max
  val part2 = heights.length

  println(part1)
  println(part2)