import language.experimental.captureChecking
import java.io.*
import caps.*

// - Capturing / escaping ----------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

object escape:
  // x is captured and escapes its intended scope.
  def createFun =
    val x = 1
    (y: Int) => x + y

  val f = createFun
  println(f(2)) // > 3

object problem:
  // try-with-resource
  def withFile[T](name: String)(f: OutputStream => T): T =
    val out    = new FileOutputStream(name) // Acquisition
    val result = f(out)

    out.close // Release
    result

  // `nasty` captures `out`.
  val nasty = withFile("log.txt")(out => (i: Int) => out.write(i))
  nasty(1) // IOException

  // secret handling
  opaque type Secret = String
  def withSecret[T](f: Secret => T): T =
    val secret = "Celui qui lit ça est un @!#"
    val result = f(secret)

    result

  // `secret` contains the escaped secret.
  val secret = withSecret(identity)

// - Capture set -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
// Just some short class name, nothing special about it.
class A

def captureSet =
  // `a1` is tracked because it captures the root capability, `cap`
  val a1: A^{cap} = A()

  // `a2` tracks `a1` and is prevented from escaping because that would allow `a1` to escape.
  val a2: A^{a1} = a1

object fixed_problem:
  // `f`'s parameter is now tracked.
  def withFile[T](name: String)(f: OutputStream^{cap} => T): T =
    val out    = new FileOutputStream(name) // Acquisition
    val result = f(out)

    out.close // Release
    result

  // No longer compiles, because `out` is correctly spotted to escape.
  // val nasty = withFile("log.txt")(out => (i: Int) => out.write(i))

  // Can no longer use a type alias because the compiler refuses to track "pure" types, such as primitive types.
  // I'm not amused.
  case class Secret(value: String)
  def withSecret[T](f: Secret^{cap} => T): T =
    val secret = Secret("Celui qui lit ça est un @!#")
    val result = f(secret)

    result

  // No longer compiles because `secret` is correctly spotted to escape.
  // val secret = withSecret(identity)

def subset =
  val a1: A^{cap} = A()
  val a2: A^{cap} = A()
  val a3: A^{a1}  = a1

  // Compiles, because {a1} is a subset of {a1, a2} and therefore A^{a1} a subtype of A^{a1, a2}.
  // This is why we can pass an `OutputStream` where an `OutputStream^{cap}` is expected in `withFile`.
  a3: A^{a1, a2}

def syntacticSugar =
  // `A^` is strictly equivalent to `A^{cap}`.
  val a1: A^ = A()

  // `A => B` is strictly equivalent to `Function[A, B]^`, and known as an impure function.
  val f1: Int => Int = identity

  // `A -> B` is strictly equivalent to `Function[A, B]`, and known as a pure function.
  val f2: Int -> Int = identity

  // `A ->c B` is strictly equivalent to `Function[A, B]^c`
  val f3: Int ->{a1} Int = identity

def higherOrder =
  // We'll need a value to track.
  val a1: A^ = A()

  val pure: Function[Int, Int]          = _ + 1
  val tracking: Function[Int, Int]^{a1} = pure

  // `map`, if `A => B` meant a pure function.
  def pureMap(f: Function[Int, Int]) = List(1, 2, 3).map(f)
  pureMap(pure)
  // pureMap(tracking) // This does not compile, because the subtyping relation is upside down.

  // `map`, if `A => B` meant impure function.
  def impureMap(f: Function[Int, Int]^) = List(1, 2, 3).map(f)
  impureMap(pure)
  impureMap(tracking)

def transitivity =
  val a1: A^     = A()
  val a2: A^{a1} = a1
  val a3: A^{a2} = a2

  // This compiles, because tracking is transitive.
  // We've not actually lost a tracked value: `a2` was only tracked because it captured `a1`. So long as we're still
  // tracking `a1`, we're fine.
  a3: A^{a1}

// - Capturing values --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
def capturingFunction =
  val a1: A^ = A()

  // `f` captures `a1`, a tracked, free variable it explicitly refers to in its body.
  val f: Int ->{a1} Int =
    x =>
      println(a1)
      x + 1

def capturingClasses =
  val a1: A^ = A()
  case class Wrapper(value: A^)

  // `w` tracks `a1` because it stores it.
  val w: Wrapper^{a1} = Wrapper(a1)

def capturingPoly =
  val a1: A^ = A()
  case class Wrapper[T](value: T, i: Int)

  // `w` does not track `a1`! But `w.value` does.
  val w: Wrapper[A^{a1}] = Wrapper(a1, 1)

  // `w.i` is not tracked, no reason to update the function's capture set.
  val nonTracking: Int -> Int =
    x =>
      println(w.i)
      x + 1

  // `w.value` is tracked, we must update the function's capture set.
  val tracking: Int ->{a1} Int =
    x =>
      println(w.value)
      x + 1

// - Catching undesired escapes ----------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------

val nasty = fixed_problem.withFile("log.txt"):
  out =>

    // `f` captures `out`, which doesn't exist outside of the scope of this function.
    val f: Int ->{out} Unit = i => out.write(i)

    // This is rejected because the compiler goes through the avoidance process to find an acceptable type for `f` and
    // lands on `Int ->{cap} Unit`. Type parameters being inferred to capturing `cap` are treated as a value escaping.
    // f

object avoidance:
  trait Animal

  // This cannot be inferred to have type `Cat`, since `Cat` doesn't exist oustide of the boundaries of `createCat`.
  // Instead, the compiler widens the type until it finds an acceptable one: `Animal`
  def createCat =
    case class Cat(lives: Int) extends Animal
    Cat(9)

  val cat: Animal = createCat
