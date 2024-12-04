package operators

import models.Convolution2D
import models.Pooling2D
import models.Dense
import models.Sequencer
import models.ConvolutionalNeuralNetwork
import models.Input2D
import models.Layer2D

trait Enumerable[T] {
  def countRegisters(l: T): Int = 0
  def countRegistersOptim(l: T): Int = 0
  def countMultipliers(l: T): Int = 0
  def countAdders(l: T): Int = 0
  def countReLUs(l: T): Int = 0
  def countMax(l: T): Int = 0
  def countFlows(l: T): Int = 0
  def countFlowsOptim(l: T): Int = 0
}

object Enumerable {
  private def nextrRegisterCount(win: Int, hker: Int, wker: Int) =
    (hker - 1) * win + wker

  trait Instances {
    implicit class EnumerableOps[T](unit: T)(implicit enumerable: Enumerable[T]) extends Enumerable[T]{
      def enumerate: Map[String, Int] = {
        Map(
          "reg" -> enumerable.countRegisters(unit),
          "mult" -> enumerable.countMultipliers(unit),
          "add" -> enumerable.countAdders(unit),
          "max" -> enumerable.countMax(unit),
          "ReLU" -> enumerable.countReLUs(unit),
          "flows" -> enumerable.countFlows(unit),
          "optiFlows" -> enumerable.countFlowsOptim(unit)
        )
      }
      def enumerateOptim: Int = enumerable.countRegistersOptim(unit)
    }

    implicit val convolution2DIsEnumerable: Enumerable[Convolution2D]
    = new Enumerable[Convolution2D] {

      override def countAdders(unit: Convolution2D): Int = unit.filters

      override def countMultipliers(unit: Convolution2D): Int =
        unit.kernel_size.height * unit.kernel_size.width * unit.filters

      override def countReLUs(unit: Convolution2D): Int = unit.filters


      override def countRegisters(unit: Convolution2D): Int = unit.inputShape.channel * nextrRegisterCount(
        unit.inputShape.width,
        unit.kernel_size.height,
        unit.kernel_size.width
      ) + unit.filters * (unit.kernel_size.height * unit.kernel_size.width + 1)

      override def countRegistersOptim(unit: Convolution2D): Int = nextrRegisterCount(
        unit.inputShape.width,
        unit.kernel_size.height,
        unit.kernel_size.width
      ) +  2

      // count every flows except the inputs
      override def countFlows(unit: Convolution2D): Int =
        countRegisters(unit) + (unit.kernel_size.width*unit.kernel_size.height*unit.filters) + countMultipliers(unit) * 2 + countAdders(unit) * 2 + countReLUs(unit)

      override def countFlowsOptim(unit: Convolution2D): Int =
        ((unit.kernel_size.height - 1) * unit.inputShape.width + unit.kernel_size.width)+ 1 + 2 // only the output flows of registers for a single input/output channel
    }

    implicit val pooling2DIsEnumerable: Enumerable[Pooling2D] = new Enumerable[Pooling2D] {
      override def countRegisters(unit: Pooling2D): Int = unit.inputShape.channel * (nextrRegisterCount(
        unit.inputShape.width,
        unit.kernel_size.height,
        1
      ) + nextrRegisterCount(1, unit.kernel_size.width, 1))

      override def countRegistersOptim(unit: Pooling2D): Int = (nextrRegisterCount(
        unit.inputShape.width,
        unit.kernel_size.height,
        1
      ) + nextrRegisterCount(1, unit.kernel_size.width, 1))

      override def countFlows(unit: Pooling2D): Int =
        countRegisters(unit) + unit.inputShape.channel * (unit.kernel_size.height + unit.kernel_size.width + 2)

      override def countFlowsOptim(unit: Pooling2D): Int = countRegisters(unit) / unit.inputShape.channel + 2

      override def countMax(unit: Pooling2D): Int = 2 * unit.inputShape.channel
    }
    implicit val denseIsEnumerable: Enumerable[Dense] = new Enumerable[Dense] {
      override def countAdders(unit: Dense): Int = 2 * unit.outputShape.channel
      override def countMultipliers(unit: Dense): Int = unit.inputShape.channel * unit.outputShape.channel
      override def countFlows(unit: Dense): Int = unit.outputShape.channel * (3 * unit.inputShape.channel + 6)
      override def countFlowsOptim(unit: Dense): Int = 3
      override def countRegisters(unit: Dense): Int = (2 + unit.inputShape.channel) * unit.outputShape.channel
      override def countRegistersOptim(unit: Dense): Int = 3
    }

    implicit val sequencerIsEnumerable: Enumerable[Sequencer] = new Enumerable[Sequencer] {
      override def countFlows(unit: Sequencer): Int =
        unit.outputShape.channel * ((unit.inputShape.channel - 1) * 2 + 1)
      override def countFlowsOptim(unit: Sequencer): Int = unit.inputShape.channel
      override def countRegisters(unit: Sequencer): Int = (unit.inputShape.channel) * unit.outputShape.channel
      override def countRegistersOptim(unit: Sequencer): Int = unit.inputShape.channel
    }

    implicit val input2DIsEnumerable: Enumerable[Input2D] = new Enumerable[Input2D] {

      override def countFlows(l: Input2D): Int = l.outputShape.channel

      override def countFlowsOptim(l: Input2D): Int = 1

    }


    implicit class SeqOfEnumerableIsEnumerable[T](enumerable: Enumerable[T]) extends Enumerable[Seq[T]] {

      override def countRegisters(units: Seq[T]): Int = units.map(l => enumerable.countRegisters(l)).sum
      override def countRegistersOptim(units: Seq[T]): Int = units.map(l => enumerable.countRegistersOptim(l)).sum

      override def countMultipliers(units: Seq[T]): Int = units.map(l => enumerable.countMultipliers(l)).sum

      override def countAdders(units: Seq[T]): Int = units.map(l => enumerable.countAdders(l)).sum

      override def countReLUs(units: Seq[T]): Int = units.map(l => enumerable.countReLUs(l)).sum

      override def countMax(units: Seq[T]): Int = units.map(l => enumerable.countMax(l)).sum

      override def countFlows(units: Seq[T]): Int = units.map(l => enumerable.countFlows(l)).sum

      override def countFlowsOptim(units: Seq[T]): Int = units.map(l => enumerable.countFlowsOptim(l)).sum

    }

  }

}
