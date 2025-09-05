package rand

trait Rand:
  def range(min: Int, max: Int): Int

def range(min: Int, max: Int): Rand ?=> Int =
  r ?=> r.range(min, max)

def nextInt(max: Int): Rand ?=> Int =
  range(0, max)

def system[A](a: Rand ?=> A): A =
  val handler = new Rand:
    val r = new scala.util.Random

    def range(min: Int, max: Int) =
      r.nextInt(max - min) + min

  a(using handler)

def const[A](notRandom: Int)(ra: Rand ?=> A): A =
  val handler = new Rand:
    override def range(min: Int, max: Int) =
      notRandom

  ra(using handler)
