package spire.benchmark

import scala.reflect.ClassTag

import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.util.Random
import Random._

import spire.algebra._
import spire.math._
import spire.implicits._
import fpf._

import com.google.caliper.Runner 
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import java.lang.Math

object NaturalBenchmarks extends MyRunner(classOf[NaturalBenchmarks])

class NaturalBenchmarks extends MyBenchmark {
  @Param(Array("8", "16", "32", "64", "96", "128", "192", "256"))
  var bits: Int = 0

  var size: Int = 0

  @Param(Array("10", "15", "20"))
  var pow: Int = 0

  var nats: Array[Natural] = _
  var bigints: Array[BigInt] = _

  override def setUp() {
    size = Math.pow(2, pow).toInt
    bigints = init(size)(BigInt(bits, Random))
    nats = bigints.map(Natural(_))
  }

  def timeNaturalSum(reps: Int) = run(reps) {
    var sum = Natural(0)
    nats.foreach(n => sum += n)
  }

  def timeBigIntSum(reps: Int) = run(reps) {
    var sum = BigInt(0)
    bigints.foreach(n => sum += n)
  }
}
