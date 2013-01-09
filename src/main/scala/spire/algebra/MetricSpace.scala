package spire.algebra

import scala.{ specialized => spec }

trait MetricSpace[V, @spec(Int, Long, Float, Double) R] {
  def distance(v: V, w: V): R
}

object MetricSpace
