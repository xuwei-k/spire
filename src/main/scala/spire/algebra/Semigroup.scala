package spire.algebra

import scala.{specialized => spec}

import scala.{ specialized => spec }

/**
 * A semigroup is any set `A` with an associative operation (`op`).
 */
trait Semigroup[@spec(Int,Long,Float,Double) A] {
  def op(x:A, y:A): A
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]) = s
}

final class SemigroupOps[@spec(Int,Long,Float,Double) A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = ev.op(lhs, rhs)
}
