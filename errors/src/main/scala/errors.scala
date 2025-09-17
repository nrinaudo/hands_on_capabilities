package errors

import language.experimental.captureChecking
import scala.util.control.*
import scala.caps.*
import scala.caps.unsafe.unsafeAssumePure

// - "Classic" style ----------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------------------------
def sequenceOld[A](oas: List[Option[A]]): Option[List[A]] =
  val flipped = oas.foldLeft(Option(List.empty[A])):
    case (None, _) | (_, None)  => None
    case (Some(acc), Some(cur)) => Some(cur :: acc)

  flipped.map(_.reverse)


// // - Label capability -----------------------------------------------------------------------------------------------
// // ------------------------------------------------------------------------------------------------------------------

// Break is flagged as private because it's doing unsavory things with capture sets and is an implementation detail
// we don't want anyone to have access to.
private case class Break[A](label: Label[A]^{}, value: A) extends ControlThrowable with NoStackTrace

// Note the `sealed abstract` bit: that is to make sure no one but us can create and instance of `Label`.
// We want to keep control of that, the only place where one should be able to create a `Label` is in `break`.
sealed abstract class Label[E] extends SharedCapability

// unsafeAssumePure is needed here to convince the compiler that it's fine to pass a `Label[A]^{cap}` where a
// `Label[A]` is expected.
def break[A](a: A): Label[A] ?=> Nothing =
  label ?=> throw Break(label.unsafeAssumePure, a)

def break: Label[Unit] ?=> Nothing =
  break(())

def handle[E, S, A](
  la     : Label[E] ?=> S,
  success: S => A,
  error  : E => A
): A =
  val label = new Label[E] {}

  try success(la(using label))
  catch case Break(`label`, value) => error(value)


def boundary[A](ba: Label[A] ?=> A): A =
  handle(ba, identity, identity)

// // - Options --------------------------------------------------------------------------------------------------------
// // ------------------------------------------------------------------------------------------------------------------
extension [A](oa: Option[A]) def ? : Label[Unit] ?=> A =
  oa match
    case Some(a) => a
    case None    => break

def option[E, S](os: Label[E] ?=> S): Option[S] =
  handle(os, Option.apply, _ => None)

def sequenceOpt[A](oas: List[Option[A]]): Option[List[A]] =
  option:
    oas.map(_.?)


// - eithers ------------------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------------------------
def either[E, S](es: Label[E] ?=> S): Either[E, S] =
  handle(es, Right.apply, Left.apply)

extension [E, A](ea: Either[E, A]) def ? : Label[E] ?=> A =
  ea match
    case Right(a) => a
    case Left(e) => break(e)

def sequenceEither[E, A](eas: List[Either[E, A]]): Either[E, List[A]] =
  either:
    eas.map(_.?)

// - Nested -------------------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------------------------

def sequencePositive(ois: List[Option[Int]]): Either[String, Option[List[Int]]] = {
  either: fail ?=>
    option:
      ois.map: oi =>
        val i = oi.?
        if i >= 0 then i
        else break(s"Negative number: $i")(using fail)
}

// - Capture checking --------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------------------------

// The following does not compile, because `Iterator` is lazy and captures the `Label`, and `Label` is flagged as a
// `Capability`.
// def broken(is: Iterator[Option[Int]]): Option[Iterator[Int]] =
//   option:
//     is.map(_.?)

// - Generic sequencing -------------------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------------------------------------
// This demonstrates how to write a generic `sequence` implementation using this mechanism, by using type classes.

// Ability to map into some higher kinded type.
trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

object Functor:
  given Functor[List] with
    extension [A](fa: List[A]) def map[B](f: A => B) = fa.map(f)

// Ability to lift a value into some higher kinded type.
trait Lift[F[_]]:
  extension [A](a: A) def lift: F[A]

object Lift:
  given Lift[Option]:
    extension [A](a: A ) def lift = Some(a)

  given [X] => Lift[Either[X, *]]:
    extension [A](a: A ) def lift = Right(a)

// Ability to unwrap the value contained by some higher kinded type as an effectful computation.
trait Unwrap[F[_]]:
  extension [A](fa: F[A]) def ?[B]: Label[F[B]] ?=> A

object Unwrap:
  given Unwrap[Option]:
    extension [A](oa: Option[A]) def ?[B] =
      oa match
        case Some(a) => a
        case None    => break(Option.empty)

  given [X] => Unwrap[Either[X, *]]:
    extension [A](ea: Either[X, A]) def ?[B] =
      ea match
        case Right(a) => a
        case Left(x)  => break(Left(x): Either[X, B])

// Prompt specifically to allow `_.?`.
def unwrap[F[_]: Lift, A](fa: Label[F[A]] ?=> A): F[A] =
  handle(fa, _.lift, identity)

// Putting it all together.
def sequenceGeneric[F[_]: Functor, G[_]: Unwrap: Lift, A](fga: F[G[A]]): G[F[A]] =
  unwrap:
    fga.map(_.?)
