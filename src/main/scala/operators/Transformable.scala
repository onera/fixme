package operators

import com.fasterxml.jackson.module.scala.deser.overrides.LazyList
import models._
import operators.Transformable.TFlow
import operators.all._

import scala.language.reflectiveCalls

trait Transformable[T, I] {
  type Result
  def transform(x: T): TFlow[I] => Result
}

object Transformable {

  type Aux[T, I, R] = Transformable[T,I] {
    type Result = R
  }

  type TFlow[T] = LazyList[T]

  case class Flow[T](id: String, tFlow: TFlow[T]) {
    override def toString: String = s"$id = ${tFlow.toList}"
  }

  sealed abstract class Component[T] {
    val id: String
    val inputs: Array[Flow[T]]
    val outputs: Array[Flow[T]]
  }

  sealed abstract class SingleInputComponent[T] extends Component[T]{
    val id: String
    val input: Flow[T]
    val inputs: Array[Flow[T]] = Array(input)
  }

  sealed abstract class HierarchicalComponent[T] extends SingleInputComponent[T]{
    val output: Flow[T]
    val registers:Seq[Register[T]]
  }

  abstract class Register[T](inputTFlow: TFlow[T]) extends SingleInputComponent[T]

  case class SimpleRegister[T](id: String, inputTFlow: TFlow[T], output: Flow[T]) extends Register[T](inputTFlow) {
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)
    val outputs: Array[Flow[T]] = Array(output)
  }

  case class ShiftRegisterUnit[T:Streamable](id: String, inputTFlow: TFlow[T], size: Int, needed: Array[Int]) extends HierarchicalComponent[T] {

    require(size >= 1, s"The size of a shift register is always geq to 1")

    val input: Flow[T] = Flow(s"$id.i", inputTFlow)

    private val outputFlows = (0 until size).map(i => Flow(s"$id.r$i.o", inputTFlow.shift(needed, i + 1)))

    val registers: Seq[SimpleRegister[T]] =
      (0 until size).toArray.map(
        i => SimpleRegister(s"r$i", if (i == 0) inputTFlow else outputFlows(i - 1).tFlow, outputFlows(i)))

    val output: Flow[T] = registers.last.output.copy(id = s"$id.o")
    val outputs: Array[Flow[T]] = Array(output)
  }

  case class Extractor[T:Streamable](id: String, inputTFlow: TFlow[T], shiftRegisters: Array[ShiftRegisterUnit[T]]) extends HierarchicalComponent[T] {

    val input: Flow[T] = Flow(s"$id.i", inputTFlow)

    val registers: Seq[SimpleRegister[T]] = implicitly[Streamable[T]].merge(id, shiftRegisters)

    val output: Flow[T] = shiftRegisters.head.output.copy(id = s"$id.o")

    val outputs: Array[Flow[T]] = shiftRegisters
      .sortBy(_.size)
      .map(s => s.output.copy(id = s"$id.o${s.size - 1}"))
  }

  object NeighbourExtractor {
    def apply[T:Streamable](id: String, inputTFlow: TFlow[T], hs: Int, ws: Int, hp: Int, wp: Int, hker: Int, wker: Int, hin: Int, win: Int): Extractor[T] = {
      val wout = (win + 2 * wp - wker) / ws + 1
      val hout = (hin + 2 * hp - hker) / hs + 1
      val size = (hker - 1) * win + wker
      val shiftSizes = (for{k <- 0 until wker; l <- 0 until hker} yield k + l * win).sorted
      val usedIndices = shiftSizes
        .map(p => (0 until wout).flatMap(c => (0 until hout).map(l => (size - p - 1) + ws * c + l * hs * win)).sorted)
      val shiftRegisters = shiftSizes
        .indices
        .map(i => ShiftRegisterUnit(s"$id.shift${shiftSizes(i)}", inputTFlow, shiftSizes(i) + 1, usedIndices(i).toArray))
      Extractor(id, inputTFlow, shiftRegisters.toArray)
    }
  }

  object VerticalExtractor {
    def apply[T:Streamable](id: String, inputTFlow: TFlow[T], hs: Int, hpool: Int, hin: Int, win: Int): Extractor[T] =
      NeighbourExtractor(id, inputTFlow, hs, 1, 0, 0, hpool, 1, hin, win)
  }

  object HorizontalExtractor {
    def apply[T:Streamable](id: String, inputTFlow: TFlow[T], ws: Int, wpool: Int, hin: Int, win: Int): Extractor[T] =
      NeighbourExtractor(id, inputTFlow, 1, ws, 0, 0, 1, wpool, hin, win)
  }

  case class ConvolutionChannelUnit[T:Streamable](id: String, inputTFlow: TFlow[T]) extends HierarchicalComponent[T] {
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)
    val prodRegister: SimpleRegister[T] = SimpleRegister(
      s"$id.prodRegister",
      inputTFlow,
      Flow(s"$id.prodRegister.o",inputTFlow.shift(1)))
    val addRegister: SimpleRegister[T] = SimpleRegister(
      s"$id.addRegister",
      prodRegister.output.tFlow,
      Flow(s"$id.addRegister.o",  prodRegister.output.tFlow.shift(1)))
    val output: Flow[T] = addRegister.output.copy(id = s"$id.o")
    val outputs: Array[Flow[T]] = Array(output)
    val registers: Seq[SimpleRegister[T]] = Array(prodRegister,addRegister)
  }

  case class DenseChannelUnit[T:Streamable](id: String, inputTFlow: TFlow[T]) extends HierarchicalComponent[T] {
    self =>
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)
    val prodRegister: SimpleRegister[T] = SimpleRegister(
      s"$id.prodRegister",
      inputTFlow,
      Flow(s"$id.prodRegister.o", inputTFlow.shift(1)))
    //WARNING THERE ARE TWO FLOWS FOR THE OUTPUTS OF THE ACCUMULATION REGISTER BUT THERE IS ONLY ONE OUTPUT PORT
    //WHICH IS RESULT OUT, SO SA ARE ONLY ON THE OUTPUT PORT
    val accRegister: Register[T] {val resultOut: Flow[T]; val accOut:Flow[T]} = new Register[T](prodRegister.output.tFlow) {
      val id: String = s"${self.id}.accRegister"
      val input: Flow[T] = Flow(s"$id.i",inputTFlow)
      val accOut: Flow[T] = Flow(
        s"$id.acc",
        if(prodRegister.output.tFlow.nonEmpty) prodRegister.output.tFlow.init.shift(1) else Stream.empty )
      val resultOut: Flow[T] = Flow(
        s"$id.o",
        prodRegister.output.tFlow.shift(
          if(prodRegister.output.tFlow.nonEmpty) Array(prodRegister.output.tFlow.indices.last) else Array.empty,
          1))
      val outputs: Array[Flow[T]] = Array(resultOut, accOut)
    }
    val outRegister: SimpleRegister[T] = SimpleRegister(
      s"$id.outRegister",
      accRegister.resultOut.tFlow,
      Flow(s"$id.outRegister.o", accRegister.resultOut.tFlow.shift(1 )))
    val registers: Seq[Register[T]] = Array(
      prodRegister,
      accRegister,
      outRegister)
    val output: Flow[T] = outRegister.output.copy(id = s"$id.o")
    val outputs: Array[Flow[T]] = Array(output)
  }

  case class PoolingLayerUnit[T:Streamable](id: String, inputTFlow: TFlow[T], hs: Int, ws: Int, hpool: Int, wpool: Int, hin: Int, win: Int, channels: Int) extends HierarchicalComponent[T] {
    outer =>
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)
    val verticalExtractor: Extractor[T] = VerticalExtractor(s"$id.verticalExtractor", inputTFlow, hs, hpool, hin, win)
    val horizontalExtractor: Extractor[T] = HorizontalExtractor(
      s"$id.horizontalExtractor",
      verticalExtractor.output.tFlow,
      ws,
      wpool,
      (hin - hpool) / hs + 1,
      win)
    val output: Flow[T] = horizontalExtractor.output.copy(id = s"$id.o")
    val outputs: Array[Flow[T]] = Array(output)
    val registers: Seq[SimpleRegister[T]] = verticalExtractor.registers ++ horizontalExtractor.registers
    val verticalMax: Component[T] = new Component[T] {
      val id: String = s"${outer.id}.vMax"
      val inputs: Array[Flow[T]] = verticalExtractor.outputs.indices.map(i => verticalExtractor.outputs.head.copy(id = s"$id.i_$i")).toArray
      val outputs: Array[Flow[T]] = Array(verticalExtractor.outputs.head.copy(id = s"$id.o"))
    }
    val horizontalMax: Component[T] = new Component[T] {
      val id: String = s"${outer.id}.hMax"
      val inputs: Array[Flow[T]] = horizontalExtractor.outputs.indices.map(i => horizontalExtractor.outputs.head.copy(id = s"$id.i_$i")).toArray
      val outputs: Array[Flow[T]] = Array(horizontalExtractor.outputs.head.copy(id = s"$id.o"))
    }
  }

  case class ConvolutionLayerUnit[T:Streamable](id: String, inputTFlow: TFlow[T], hs: Int, ws: Int, hp: Int, wp: Int, hker: Int, wker: Int, hin: Int, win: Int, cin: Int=1,cout: Int=1) extends HierarchicalComponent[T] {
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)
    val neighbourExtractor: Extractor[T] = NeighbourExtractor(s"$id.extractor", inputTFlow, hs, ws, hp, wp, hker, wker, hin, win)
    val convolutionChannelUnit: ConvolutionChannelUnit[T] = ConvolutionChannelUnit(s"$id.dotProduct", neighbourExtractor.output.tFlow)
    val output: Flow[T] = convolutionChannelUnit.output.copy(id = s"$id.o")
    val outputs: Array[Flow[T]] = Array(output)
    val registers: Seq[SimpleRegister[T]] = neighbourExtractor.registers ++ convolutionChannelUnit.registers
  }

  case class SequencerLayerUnit[T:Streamable](id: String, inputTFlow: TFlow[T], cin: Int, cout: Int) extends HierarchicalComponent[T] {
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)

    private val shiftRegisters = (0 until cin / cout).toArray
      .map(i => ShiftRegisterUnit(s"$id.shiftRegister$i", inputTFlow, i + 1, if(inputTFlow.nonEmpty) inputTFlow.indices.toArray else Array.empty))

    private val mapping = (for {
      s <- shiftRegisters.indices
      r <- shiftRegisters(s).registers.indices
    } yield {
      (s, r) -> (r + cin / cout - s)
    }).toMap

    val registers: Seq[SimpleRegister[T]] = implicitly[Streamable[T]].merge(id, shiftRegisters, mapping)

    val output: Flow[T] = registers.last.output.copy(id = s"$id.o")

    val outputs: Array[Flow[T]] = Array(output)
  }

  case class CnnStreamingAccelerator[T: Streamable](id: String, inputTFlow: TFlow[T], layers: Seq[HierarchicalComponent[T]]) extends HierarchicalComponent[T]{
    val input: Flow[T] = Flow(s"$id.i", inputTFlow)
    val output: Flow[T] = layers.last.registers.last.outputs(0).copy(id = s"$id.o")
    val outputs: Array[Flow[T]]=Array(output)
    val registers: Seq[Register[T]] = layers.foldLeft(Seq.empty[Register[T]]){(seq,layer) =>
     seq ++ layer.registers
    }
  }

  trait Instances {

    implicit class TransformableOps[T](x: T) {
      def transform[I, U](f: TFlow[I])(implicit e: Aux[T, I, U]): U = e.transform(x)(f)
    }

    implicit def poolingLayerIsTransformable[T: Streamable]: Aux[Pooling2D, T, PoolingLayerUnit[T]] = new Transformable[Pooling2D, T] {
      type Result = PoolingLayerUnit[T]

      def transform(x: Pooling2D): TFlow[T] => PoolingLayerUnit[T] = f => PoolingLayerUnit(x.name, f, x.strides.height, x.strides.width, x.kernel_size.height, x.kernel_size.width, x.inputShape.height, x.inputShape.width,x.inputShape.channel)
    }

    implicit def convLayerIsTransformable[T: Streamable]: Aux[Convolution2D, T, ConvolutionLayerUnit[T]] = new Transformable[Convolution2D, T] {
      type Result = ConvolutionLayerUnit[T]

      def transform(x: Convolution2D): TFlow[T] => ConvolutionLayerUnit[T] = f => ConvolutionLayerUnit(x.name, f, x.strides.height, x.strides.width, x.padding_size.height, x.padding_size.width, x.kernel_size.height, x.kernel_size.width, x.inputShape.height, x.inputShape.width,x.inputShape.channel,x.outputShape.channel)
    }

    implicit def sequencerIsTransformable[T: Streamable]: Aux[Sequencer, T, SequencerLayerUnit[T]] = new Transformable[Sequencer, T] {
      type Result = SequencerLayerUnit[T]

      def transform(x: Sequencer): TFlow[T] => SequencerLayerUnit[T] = f => SequencerLayerUnit(x.name, f, x.inputShape.channel, x.outputShape.channel)
    }

    implicit def denseIsTransformable[T: Streamable]: Aux[Dense, T, DenseChannelUnit[T]] = new Transformable[Dense, T] {
      type Result = DenseChannelUnit[T]

      def transform(x: Dense): TFlow[T] => DenseChannelUnit[T] = f => DenseChannelUnit(x.name, f)
    }

    implicit def inputIsTransformable[T: Streamable]: Aux[Input2D, T, TFlow[T]] = new Transformable[Input2D, T] {
      type Result = TFlow[T]

      def transform(x: Input2D): TFlow[T] => TFlow[T] = _ =>
        (0 to x.inputShape.height * x.inputShape.width + 1).toStream.map(implicitly[Streamable[T]].genValue)
    }

    implicit def convolutionalNeuralNetworkIsTransformable[T: Streamable]: Aux[ConvolutionalNeuralNetwork, T, CnnStreamingAccelerator[T]] = new Transformable[ConvolutionalNeuralNetwork, T] {
     type Result = CnnStreamingAccelerator[T]
      def transform(x: ConvolutionalNeuralNetwork): TFlow[T] => CnnStreamingAccelerator[T] = flow => {
        val layerUnits = x.layers.drop(0).foldLeft(Seq.empty[HierarchicalComponent[T]]) {
          (seqLayer, layer) =>
            val input = if (seqLayer.isEmpty) flow else seqLayer.last.output.tFlow
            (for {
             res <- layer match {
              case l: Convolution2D => Some(l.transform(input))
              case l: Pooling2D => Some(l.transform(input))
              case l: Dense => Some(l.transform(input))
              case l: Sequencer => Some(l.transform(input))
              case _ => None
            }
            } yield {
              seqLayer:+ res
            }).getOrElse(seqLayer)
        }
        CnnStreamingAccelerator(x.name,flow,layerUnits)
      }
    }
  }

}
