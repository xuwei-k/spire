package spire.algebra

import scala.{ specialized => spec }

trait NormedVectorSpace[V, @spec(Int, Long, Float, Double) F]
extends VectorSpace[V, F] with MetricSpace[V, F] {
  def norm(v: V): F

  def normalize(v: V): V = divr(v, norm(v))
  def distance(v: V, w: V): F = norm(minus(v, w))
}

trait NormedVectorSpace0 {
  implicit def InnerProductSpaceIsNormedVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: InnerProductSpace[V, F]): NormedVectorSpace[V, F] = vectorSpace
}

object NormedVectorSpace extends NormedVectorSpace0

final class NormedVectorSpaceOps[V](lhs: V) {
  def norm[F](implicit ev: NormedVectorSpace[V, F]): F = ev.norm(lhs)
  def normalize[F](implicit ev: NormedVectorSpace[V, F]): V = ev.normalize(lhs)
}
