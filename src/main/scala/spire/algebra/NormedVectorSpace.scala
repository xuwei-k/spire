package spire.algebra

import scala.{ specialized => spec }

trait NormedVectorSpace[V, @spec(Int, Long, Float, Double) F]
extends VectorSpace[V, F] with MetricSpace[V, F] {
  def norm(v: V): F

  def normalize(v: V): V = divr(v, norm(v))
  def distance(v: V, w: V): F = norm(minus(v, w))
}

object NormedVectorSpace extends NormedVectorSpace0

trait NormedVectorSpace0 {
  implicit def InnerProductSpaceIsNormedVectorSpace[V, @spec(Int, Long, Float, Double) F](implicit
    vectorSpace: InnerProductSpace[V, F]): NormedVectorSpace[V, F] = vectorSpace
}

final class NormedVectorSpaceOps[V](lhs: V) {
  def norm[@spec(Int,Long,Float,Double) F](implicit ev: NormedVectorSpace[V, F]): F = ev.norm(lhs)
  def normalize[@spec(Int,Long,Float,Double) F](implicit ev: NormedVectorSpace[V, F]): V = ev.normalize(lhs)
}
