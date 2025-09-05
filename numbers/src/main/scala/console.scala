package console

import print.Print
import read.Read

trait Console extends Print with Read

val handler = new Console {
  override def readLine() =
    Console.in.readLine

  override def println(s: String) =
    Console.println(s)
}

def apply[A](a: Console ?=> A): A = a(using handler)
