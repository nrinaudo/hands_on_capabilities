//> using scala 3.7
//> using option -language:experimental.captureChecking -nowarn

import scala.util.Random.nextInt
import scala.io.StdIn.readLine

def readInput: Option[Int] =
  try Some(readLine.toInt)
  catch case _ => None

def guess(target: Int): Boolean =
  readInput match
    case Some(`target`) => true

    case Some(input) =>
      if input < target then println("Too low")
      else println("Too high")
      false

    case None =>
      println("Not a valid number")
      false

def loop(target: Int, remaining: Int): Boolean =
  if guess(target) then
    println("Congratulations!")
    true
  else if remaining < 0 then
    println("No more attempts")
    false
  else
    if remaining == 0 then println("Last attempt")
    else println(s"$remaining attempts remaining")
    loop(target, remaining - 1)

def run: Boolean =
  val target = nextInt(10)

  println("Input a value between 0 and 10:")
  loop(target, 2)

run
