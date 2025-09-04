trait Print:
  def println(s: String): Unit

def println(a: Any): Print ?=> Unit =
  p ?=> p.println(a.toString)

def ignore[A](pa: Print ?=> A): A =
  val handler = new Print:
    override def println(s: String) =
      ()

  pa(using handler)
