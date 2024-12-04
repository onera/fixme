package operators

import models.{Convolution2D, ConvolutionalNeuralNetwork, Dense, Input2D, Layer2D, LeNetFixture, Pooling2D, Sequencer, Shape2D}
import models.LeNetFixture.LeNetFixtureSafecompPaper.customElement
import models.LeNetFixture.mkSimpleLeNet
import operators.Transformable._
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FlowExportableTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  "A neighbour extractor" should "be exportable as a flow diagram" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,2,2,4,4)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 3
    hout shouldBe 3
    val size = (hker - 1) * win + wker
    size shouldBe 6
    val result = NeighbourExtractor(s"extractor",customElement,hs,ws, hp,wp,hker,wker,hin,win)
    result.exportFlowsDiagram
  }

  "A neighbour extractor of size 5" should "be exportable as a flow diagram" in {
    val (hs, ws, hp, wp, hker, wker, hin, win) = (1, 1, 0, 0, 2, 2, 3, 3)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 2
    hout shouldBe 2
    val size = (hker - 1) * win + wker
    val input = Stream(1, 4, 6, 7, 8, 9, 11, 12, 14)
    size shouldBe 5
    val result = NeighbourExtractor(s"nextr", input, hs, ws, hp, wp, hker, wker, hin, win)
    result.exportFlowsDiagram
  }
  "A neighbour extractor" should "be exportable as a list of time flow" in {
    val (hs, ws, hp, wp, hker, wker, hin, win) = (1, 1, 0, 0, 2, 2, 3, 3)
    val wout = (win + 2 * wp - wker) / ws + 1
    val hout = (hin + 2 * hp - hker) / hs + 1
    wout shouldBe 2
    hout shouldBe 2
    //    val input = (1 to 9).toStream
    val input = Stream(1, 4, 6, 7, 8, 9, 11, 12, 14)
    val size = (hker - 1) * win + wker
    size shouldBe 5
    val result = NeighbourExtractor(s"nextr", input, hs, ws, hp, wp, hker, wker, hin, win)
    result.exportTFlows
  }

  "A dot product" should "be flow exportable" in {
    val inputTFlow = Stream(13, 14, 15, 19, 20, 23, 25, 26, 27)
    val result = ConvolutionChannelUnit("dotProduct",inputTFlow.indices.map(i => Element(inputTFlow(i), i)).toStream)
    result.exportFlowsDiagram
  }

  "A convolution layer" should "be flow exportable" in  {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,2,2,4,4)
    val result = ConvolutionLayerUnit("convolution", customElement, hs, ws, hp, wp, hker, wker, hin, win, 1)
    result.exportFlowsDiagram
  }

  "A pooling layer" should "be flow exportable" in {
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val result = PoolingLayerUnit("pooling", customElement, hs, ws, hpool, wpool, hin, win, 1)
    result.exportFlowsDiagram
  }

  "A fully connected layer" should "be flow exportable" in {
    val result = DenseChannelUnit(s"fullyConnected",customElement)
    result.exportFlowsDiagram
  }

  "A sequencer" should "reproduce the expected pattern for custom example" in {
        val inputTFlow = Stream(1)
        val sequencer = SequencerLayerUnit("sequence",inputTFlow,9,1)
    sequencer.exportFlowsDiagram
  }
}

class ManuscriptCh4Test extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {
  val inputSmall = Stream(1, 4, 6, 7, 8, 9, 11, 12, 14)
  val inputSmallClu = Stream(9, 11, 14, 15)
  val inputLenet = (1 to 32 * 32).toStream
  "A neighbour extractor" should "be exportable as flows" in {
    val nextr = NeighbourExtractor("nextr", inputSmall, 1, 1, 0, 0, 2, 2, 3, 3)
    nextr.outputs.length shouldBe 2 * 2
    val flows = nextr.registers.flatMap(_.outputs)
    val nbAlive = flows.map(_.tFlow.size).sum
    println(nbAlive)
    nextr.exportTFlows
  }
  "A neighbour extractor of size 4" should "be exportable as flows" in {
    val nextr = NeighbourExtractor("nextr4", customElement, 1, 1, 0, 0, 2, 2, 4, 4)
    nextr.outputs.length shouldBe 2 * 2
    val flows = nextr.registers.flatMap(_.outputs)
    val nbAlive = flows.map(_.tFlow.size).sum
    println(nbAlive)
    nextr.exportTFlows
  }

  "A convolution channel unit" should "be exportable as flows" in {
    val ccu = ConvolutionChannelUnit("ccu", inputSmallClu)
    ccu.outputs.length shouldBe 1
    val flows = ccu.registers.flatMap(_.outputs)
    val nbAlive = flows.map(_.tFlow.size).sum
    println(nbAlive)
    ccu.exportTFlows
  }

  "A small convolution layer unit" should "be exportable as flows" in {
    val clu = ConvolutionLayerUnit("clu", inputSmall, 1, 1, 0, 0, 2, 2, 3, 3, 1)
    clu.outputs.length shouldBe 1
    val flows = clu.registers.flatMap(_.outputs)
    val nbAlive = flows.map(_.tFlow.size).sum
    println(nbAlive)
    clu.exportTFlows
  }

  "A convolution from LeNet5" should "be exportable as flows" in {
    val clu = ConvolutionLayerUnit("conv1", inputLenet, 1, 1, 0, 0, 5, 5, 32, 32, 1)
    clu.outputs.length shouldBe 1
    val flows = clu.registers.flatMap(_.outputs)
    val nbAlive = flows.map(_.tFlow.toSeq.size).sum
    println(nbAlive)
    //    clu.exportTFlows
  }
  "The LeNet5 streaming accelerator" should "be exportable as flows" in {
    val lenet = mkSimpleLeNet(Input2D("input", Shape2D(32, 32), Shape2D(32, 32)))
    val flowModel = lenet.layers.drop(0).foldLeft(Seq.empty[HierarchicalComponent[Int]]) {
      (seqLayer, layer) =>
        val input = if (seqLayer.isEmpty) Flow("input", inputLenet) else seqLayer.last.output
        val res = layer match {
          case l: Convolution2D => l.transform(input.tFlow)
          case l: Pooling2D => l.transform(input.tFlow)
          case l: Dense => l.transform(input.tFlow)
          case l: Sequencer => l.transform(input.tFlow)
        }
        seqLayer :+ res
    }

    val flows = flowModel.flatMap(_.registers.flatMap(_.outputs))
    val nbAlive = flows.map(_.tFlow.size).sum
    println(nbAlive)
    //    clu.exportTFlows
  }

}
