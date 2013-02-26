//package test
//
//import spire.syntax._
//
//object Test {
//  def main(args: Array[String]) {
//    val b = collection.mutable.ArrayBuffer[() => Int]()
//    cfor(0)(_ < 3, _ + 1) { x =>
//      b += (() => x)
//    }
//    println(b)
//    println(b.map(_()).toList)
//    println(b.map(_()).toList == List(0, 1, 2))
//
//    val bb = collection.mutable.ArrayBuffer[Int]()
//    cfor(0)(_ < 3, _ + 1) { x =>
//      class A { def f = x }
//      bb += (new A().f)
//    }
//    println(bb)
//  }
//}
