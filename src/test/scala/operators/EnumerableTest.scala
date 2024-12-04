package operators

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models.{Convolution2D, Dense, Input2D, Layer2D, Pooling2D, Sequencer, Shape2D}
import operators.all._
import models.LeNetFixture._

class EnumerableTest extends AnyFlatSpec with Matchers {
  "Convolution2D" should "be enumerable" in {
    val conv = Convolution2D("CONV1", Shape2D(32, 32), Shape2D(1, 1), Shape2D(5, 5), true, 6)
    val counts = conv.enumerate
    println(counts.mkString("\n"))
  }

  "Convolution2D lenet" should "be enumerable after optimization" in {
    val conv = Convolution2D("CONV1", Shape2D(32, 32), Shape2D(1, 1), Shape2D(5, 5), true, 6)
    val counts = conv.enumerateOptim
    println(counts)
  }
  "Dense" should "be enumerable" in {
    val dense = Dense("FC1", Shape2D(5,5,16),Shape2D(1,1,120))
    val counts = dense.enumerate
    println(counts.mkString("\n"))
  }

  "small convolution2D" should "be enumerable" in {
    val conv = Convolution2D("CONV1",Shape2D(3,3),Shape2D(1,1), Shape2D(2,2),true, 1)
    val counts = conv.enumerate
    println(counts.mkString("\n"))
  }

  "small convolution2D" should "be enumerable after optimization" in {
    val conv = Convolution2D("CONV1",Shape2D(3,3),Shape2D(1,1), Shape2D(2,2),true, 1)
    val counts = conv.enumerateOptim
    println(counts)
  }

  "LeNet5" should "be enumerable" in {
    val lenet = LeNetFixtureSafecompPaper
  val counts = lenet.layers.map(l => (l.name, l match {
    case unit: Convolution2D => unit.enumerate
    case unit: Pooling2D => unit.enumerate
    case unit: Dense => unit.enumerate
    case unit: Input2D => unit.enumerate
    case unit: Sequencer => unit.enumerate
  })).toMap
    val totalCount = counts.values.reduce((l,r) => {
    val d = l.keys.map(k => (k ,l(k)+r(k)))
    d.toMap
  })
    println(counts.mapValues(_.mkString("\n")).mkString("\n\n"))
    println("\nTOTAL")
    println(totalCount.mkString("\n"))
  }

  "LeNet5" should "be enumerable after optimizations" in {
    val lenet = LeNetFixtureSafecompPaper
  val counts = lenet.layers.map(l => (l.name, l match {
    case unit: Convolution2D => unit.enumerateOptim
    case unit: Pooling2D => unit.enumerateOptim
    case unit: Dense => unit.enumerateOptim
    case unit: Input2D => unit.enumerateOptim
    case unit: Sequencer => unit.enumerateOptim
  })).toMap
    val totalCount = counts.values.sum
    println(counts.mkString("\n"))
    println("\nTOTAL")
    println(totalCount)
  }
}
