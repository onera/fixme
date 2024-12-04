package operators

import models.{
  Convolution2D,
  Dense,
  Input2D,
  LeNetFixture,
  Pooling2D,
  Sequencer,
  Shape2D
}
import models.LeNetFixture.mkSimpleLeNet
import operators.Transformable.{
  ConvolutionChannelUnit,
  ConvolutionLayerUnit,
  DenseChannelUnit,
  HierarchicalComponent,
  Extractor,
  Flow,
  NeighbourExtractor,
  PoolingLayerUnit,
  SequencerLayerUnit
}
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AliveCountableErTest extends AnyFlatSpec with should.Matchers {

  "Alive instants in a neighbour extractor" should "be countable" in {
    val input = Flow[Int]("inp", Stream(1, 2, 3, 4, 5, 6, 7, 8, 9))
    val nextr =
      NeighbourExtractor[Int]("nextr", input.tFlow, 1, 1, 0, 0, 2, 2, 3, 3)
    val er = nextr.countEr
    println(er)
  }

  "Alive instants in a convolution channel unit" should "be countable" in {
    val input = Flow[Int]("inp", Stream(1, 2, 3, 4))
    val ccu = ConvolutionChannelUnit[Int]("nextr", input.tFlow)
    val er = ccu.countEr
    println(er)
  }
  "Alive instants in a convolution layer unit" should "be countable" in {
    val input = Flow[Int]("inp", Stream(1, 2, 3, 4, 5, 6, 7, 8, 9))
    val clu = ConvolutionLayerUnit[Int](
      "clu",
      input.tFlow,
      1,
      1,
      0,
      0,
      2,
      2,
      3,
      3,
      1,
      1
    )
    val er = clu.countEr
    println(er)
  }

  "Alive instants in the first convolution of lenet5" should "be countable" in {
    val input = Flow[Int]("inp", (0 until 1024).toStream)
    val clu =
      ConvolutionLayerUnit("conv1", input.tFlow, 1, 1, 0, 0, 5, 5, 32, 32, 1, 6)
    println(clu.countEr)
  }
  "Alive instants in the lenet accelerator" should "be countable" in {
    val inputLenet = Flow[Int]("inp", (0 until 1024).toStream)
    val lenet =
      mkSimpleLeNet(Input2D("input", Shape2D(32, 32), Shape2D(32, 32)))
    val flowModel = lenet.layers.drop(0).foldLeft(Seq.empty[HierarchicalComponent[Int]]) {
      (seqLayer, layer) =>
        val input =
          if (seqLayer.isEmpty) Flow("input", inputLenet.tFlow)
          else seqLayer.last.output
        val res = layer match {
          case l: Convolution2D => l.transform(input.tFlow)
          case l: Pooling2D     => l.transform(input.tFlow)
          case l: Dense         => l.transform(input.tFlow)
          case l: Sequencer     => l.transform(input.tFlow)
        }
        seqLayer :+ res
    }
    val clu = flowModel.map({
      case l: ConvolutionLayerUnit[_] => l.countEr
      case l: PoolingLayerUnit[_]     => l.countEr
      case l: DenseChannelUnit[_]     => l.countEr
      case l: SequencerLayerUnit[_]   => l.countEr
      case _                          => 0
    })
    println(clu.sum)
  }
}
