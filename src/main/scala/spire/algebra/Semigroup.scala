package spire.algebra

/**
 * A semigroup is any set `A` with an associative operation (`op`).
 */
trait Semigroup[A] {
  def op(x:A, y:A): A
}

object Semigroup {
  @inline final def apply[A](implicit s: Semigroup[A]) = s
}

final class SemigroupOps[A](lhs:A)(implicit ev:Semigroup[A]) {
  def |+|(rhs:A) = ev.op(lhs, rhs)
}
