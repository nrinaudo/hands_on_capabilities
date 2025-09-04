//> using scala 3.7
//> using option -language:experimental.captureChecking -nowarn
//> using file rand.sc
//> using file print.sc
//> using file read.sc
//> using file console.sc

import rand.*
import print.*
import read.*

def readInput: Read ?=> Option[Int] =
  try Some(readLine.toInt)
  catch case _ => None

def guess(target: Int): (Read, Print) ?=> Boolean =
  readInput match
    case Some(`target`) => true

    case Some(input) =>
      if input < target then println("Too low")
      else println("Too high")
      false

    case None =>
      println("Not a valid number")
      false

def loop(target: Int, remaining: Int): (Read, Print) ?=> Boolean =
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

val run: (Read, Rand, Print) ?=> Boolean =
  val target = nextInt(10)

  println("Input a value between 0 and 10:")
  loop(target, 2)

rand.system:
  console:
    run
