trait Read:
  def readLine(): String

val readLine: Read ?=> String =
  r ?=> r.readLine()

def const[A](s: String)(ra: Read ?=> A): A =
  val handler = new Read:
    override def readLine() =
      s

  ra(using handler)
