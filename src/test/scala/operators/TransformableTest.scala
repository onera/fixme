package operators

import models.LeNetFixture.LeNetFixtureSafecompPaper._
import operators.Transformable._
import operators.all._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TransformableTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  val uniquePositiveInts: Gen[List[Int]] =
    Gen.containerOf[Set, Int](Gen.posNum[Int])
      .map(_.toList)
      .map(_.sorted)

  implicit val posIntNoShrink: Shrink[List[Int]] = Shrink { _: List[Int] =>  Stream.empty}

  "Shift a TFlow" should "select considered indices and use the shifted value" in {
    forAll(
      (uniquePositiveInts, "flow"),
      (uniquePositiveInts, "indices"),
      (Gen.choose(1, 25), "d"),
      minSuccessful(100)) {
      (flow, indices, d) => {
        val result = flow.toStream.shift(indices.toArray, d)
        // all values in result are correctly computed
        for (i <- result.indices) {
          if (indices(i) + d < flow.size && indices(i) + d >= 0)
            result(i) shouldBe flow(indices(i) + d)
          else
            result(i) shouldBe flow.last + indices(i) + d + 1 - flow.size
        }
        // all values in flow are correctly stored in the result
        for (i <- flow.indices if indices.contains(i)) {
          if (i + d < flow.size && i + d >= 0)
            result(indices.indexOf(i)) shouldBe flow(i + d)
          else {
            val expected = flow.last + i + d + 1 - flow.size
            val found = result(indices.indexOf(i))
            found shouldBe expected
          }
        }
      }
    }
  }

  "Shift a ElementTFlow" should "select considered indices and use the shifted value" in {
    forAll(
      (uniquePositiveInts, "flow"),
      (uniquePositiveInts, "indices"),
      (Gen.choose(1, 25), "d"),
      minSuccessful(100)) {
      (flow, indices, d) => {
        val elementFlow = flow.indices.map(i => Element(flow(i),i)).toStream
        val result =  elementFlow.shift(indices.toArray, d)
        // all values in result are correctly computed
        for (i <- result.indices) {
          if (indices(i) + d < elementFlow.size && indices(i) + d >= 0)
            result(i) shouldBe elementFlow(indices(i) + d).copy(dataId = elementFlow(indices(i)).dataId)
          else
            result(i) shouldBe Element(elementFlow.last.date + indices(i) + d + 1 - elementFlow.size,elementFlow(indices(i)).dataId)
        }
        // all values in flow are correctly stored in the result
        for (i <- elementFlow.indices if indices.contains(i)) {
          if (i + d < elementFlow.size && i + d >= 0)
            result(indices.indexOf(i)) shouldBe elementFlow(i + d).copy(dataId = elementFlow(i).dataId)
          else {
            val expected = Element(elementFlow.last.date + i + d + 1 - elementFlow.size, elementFlow(i).dataId)
            val found = result(indices.indexOf(i))
            found shouldBe expected
          }
        }
      }
    }
  }

  "Merge two ordered TFlows" should "build an ordered TFlow containing all values" in {
    forAll(
      (uniquePositiveInts, "l"),
      (uniquePositiveInts, "r"),
      minSuccessful(100)) {
      (l, r) => {
        val result = (l.toStream merge r.toStream).toList
        result shouldBe (l ++ r).distinct.sorted
      }
    }
  }

  "Merge two ordered ElementFlows" should "build an ordered ElementFlow containing all values" in {
    forAll(
      (uniquePositiveInts, "l"),
      (uniquePositiveInts, "r"),
      minSuccessful(100)) {
      (l, r) => {
        val lElement = l.indices.map(i => Element(l(i),i)).toStream
        //AVOID MULTIPLE DATE FOR SAME DATA
        val rElement = r.indices.filter(i => l.indexOf(r(i)) == i || !l.contains(r(i))).map(i => Element(r(i),i)).toStream
        val result = (lElement merge rElement).toList
        result shouldBe (lElement ++ rElement).distinct.sorted
      }
    }
  }

  "A shift register" should "output a N-shifted TFlow" in {
    forAll(
      (uniquePositiveInts, "inFlow"),
      (uniquePositiveInts, "needed"),
      (Gen.choose(1, 100), "chainSize"),
      minSuccessful(100)) {
      (inFlow, needed, chainSize) => whenever(chainSize >=1) {
        val result = ShiftRegisterUnit(s"shiftRegister",inFlow.toStream,chainSize,needed.toArray)
        result.output.tFlow shouldBe inFlow.toStream.shift(needed.toArray,chainSize)
      }
    }
  }

  "A neighbour extractor" should "output various N-shifted TFlow in custom example" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,2,2,4,4)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 3
    hout shouldBe 3
    val size = (hker - 1) * win + wker
    size shouldBe 6
    val result = NeighbourExtractor(s"extractor",customInput,hs,ws, hp,wp,hker,wker,hin,win)
    //All output flows are synchronous
    for(l <- result.outputs.indices;
        r <- result.outputs.indices
        if r != l)
      result.outputs(l).tFlow shouldBe result.outputs(r).tFlow
    //Expected register patterns are
    result.registers(0).output.tFlow shouldBe Stream(4, 5, 6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26, 27)
    result.registers(1).output.tFlow shouldBe Stream(5, 6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26, 27)
    result.registers(2).output.tFlow shouldBe Stream(6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25)
    result.registers(3).output.tFlow shouldBe Stream(9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26)
    result.registers(4).output.tFlow shouldBe Stream(10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26, 27)
    result.registers(5).output.tFlow shouldBe Stream(13, 14, 15, 19, 20, 23, 25, 26, 27)
  }

  "A neighbour extractor" should "output various N-shifted ElementFlow in custom example" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,2,2,4,4)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 3
    hout shouldBe 3
    val size = (hker - 1) * win + wker
    size shouldBe 6
    val result = NeighbourExtractor(s"extractor",customElement,hs,ws, hp,wp,hker,wker,hin,win)
    //All output flows are synchronous
    for(l <- result.outputs.indices;
        r <- result.outputs.indices
        if r != l)
      result.outputs(l).tFlow.map(_.date) shouldBe result.outputs(r).tFlow.map(_.date)
    //Expected register patterns are
    result.registers(0).output.tFlow shouldBe Stream(Element(4,0), Element(5,1), Element(6,2), Element(9,3), Element(10,4), Element(13,5), Element(14,6), Element(15,7), Element(17,8), Element(19,9), Element(20,10), Element(23,11), Element(24,12), Element(25,13), Element(26,14), Element(27,15))
    result.registers(1).output.tFlow shouldBe Stream(Element(5,0), Element(6,1), Element(9,2), Element(10,3), Element(13,4), Element(14,5), Element(15,6), Element(17,7), Element(19,8), Element(20,9), Element(23,10), Element(24,11), Element(25,12), Element(26,13), Element(27,14))
    result.registers(2).output.tFlow shouldBe Stream(Element(6,0), Element(9,1), Element(10,2), Element(13,3), Element(14,4), Element(15,5), Element(17,6), Element(19,7), Element(20,8), Element(23,9), Element(24,10), Element(25,11))
    result.registers(3).output.tFlow shouldBe Stream(Element(9,0), Element(10,1), Element(13,2), Element(14,3), Element(15,4), Element(17,5), Element(19,6), Element(20,7), Element(23,8), Element(24,9), Element(25,10), Element(26,11))
    result.registers(4).output.tFlow shouldBe Stream(Element(10,0), Element(13,1), Element(14,2), Element(15,3), Element(17,4), Element(19,5), Element(20,6), Element(23,7), Element(24,8), Element(25,9), Element(26,10), Element(27,11))
    result.registers(5).output.tFlow shouldBe Stream(Element(13,0), Element(14,1), Element(15,2), Element(19,4), Element(20,5), Element(23,6), Element(25,8), Element(26,9), Element(27,10))

    result.exportFlowsDiagram
  }

  it should "output various N-shifted TFlow in CONV1" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,5,5,32,32)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 28
    hout shouldBe 28
    val size = (hker - 1) * win + wker
    size shouldBe 133
    val result = NeighbourExtractor(s"extractor",imageInput,hs,ws, hp,wp,hker,wker,hin,win)
    //All output flows are synchronous
    for(l <- result.outputs.indices;
        r <- result.outputs.indices
        if r != l)
      result.outputs(l).tFlow shouldBe result.outputs(r).tFlow
    //Expected register patterns are
    result.output.tFlow shouldBe conv1NeighbourExtractorOutput
  }

  it should "output various N-shifted TFlow in CONV2" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,5,5,14,14)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 10
    hout shouldBe 10
    val size = (hker - 1) * win + wker
    size shouldBe 61
    val result = NeighbourExtractor(s"extractor",pool1Output,hs,ws, hp,wp,hker,wker,hin,win)
    //All output flows are synchronous
    for(l <- result.outputs.indices;
        r <- result.outputs.indices
        if r != l)
      result.outputs(l).tFlow shouldBe result.outputs(r).tFlow
    //Expected register patterns are
    result.outputs(0).tFlow shouldBe conv2NeighbourExtractorOutput
  }

  "An vertical extractor" should "output N column max" in {
    val (hs,hpool,hin,win) = (2,2,4,4)
    val wout = win
    val hout =  (hin - hpool)/hs + 1
    wout shouldBe 4
    hout shouldBe 2
    val size = (hpool - 1) * win + 1
    size shouldBe 5
    val result = VerticalExtractor(s"extractor",customInput,hs, hpool,hin,win)
    //All output flows are synchronous
    for(l <- result.outputs.indices;
        r <- result.outputs.indices
        if r != l)
      result.outputs(l).tFlow shouldBe result.outputs(r).tFlow
    result.outputs(0).tFlow shouldBe Stream(10, 13, 14, 15, 24, 25, 26, 27)
    //Expected register patterns are
    //TODO WAITING IBAN EXPECTED PATTERNS
    //    result.registers(0).output.tflow shouldBe  Stream(4, 5, 6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26, 27)
    //    result.registers(1).output.tflow shouldBe  Stream(5, 6, 9, 10, 19, 20, 23, 24)
    //    result.registers(2).output.tflow shouldBe  Stream(6, 9, 10, 13, 20, 23, 24, 25)
    //    result.registers(3).output.tflow shouldBe  Stream(9, 10, 13, 14, 23, 24, 25, 26)
    //    result.registers(4).output.tflow shouldBe  Stream(10, 13, 14, 15, 24, 25, 26, 27)
  }

  "An horizontal extractor" should "output max from N column" in {
    val inputTFlow = Stream(10, 13, 14, 15, 24, 25, 26, 27)
    val (ws,wpool,hin,win) = (2,2,2,4)
    val wout = (win - 1)/ws + 1
    val hout = hin
    wout shouldBe 2
    hout shouldBe 2
    val size = wpool
    size shouldBe 2
    val result = HorizontalExtractor(s"extractor",inputTFlow,ws, wpool,hin,win)
    //All output flows are synchronous
    for(l <- result.outputs.indices;
        r <- result.outputs.indices
        if r != l) {
      result.outputs(l).tFlow shouldBe result.outputs(r).tFlow
    }
    result.outputs(0).tFlow shouldBe Stream(14, 24, 26, 28)
    //Expected register patterns are
    result.registers(0).output.tFlow shouldBe  Stream(13, 14, 15, 24, 25, 26, 27, 28)
    result.registers(1).output.tFlow shouldBe  Stream(14, 24, 26, 28)
  }

  "A dot product" should "behave as a 2-shift register" in {
    val inputTFlow = Stream(13, 14, 15, 19, 20, 23, 25, 26, 27)
    val result = ConvolutionChannelUnit("dotProduct",inputTFlow)
    result.prodRegister.output.tFlow shouldBe Stream(14, 15, 19, 20, 23, 25, 26, 27, 28)
    result.addRegister.output.tFlow shouldBe Stream(15, 19, 20, 23, 25, 26, 27, 28, 29)
    result.output.tFlow shouldBe inputTFlow.shift(2)
  }

  "A convolution layer" should "reproduce the expected pattern for custom example" in  {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,2,2,4,4)
    val result = ConvolutionLayerUnit("convolution", customInput, hs, ws, hp, wp, hker, wker, hin, win, 1)
    result.output.tFlow shouldBe Stream(15, 19, 20, 23, 25, 26, 27, 28, 29)
  }

  it should  "reproduce the expected pattern for CONV1 input" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,5,5,32,32)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 28
    hout shouldBe 28
    val size = (hker - 1) * win + wker
    size shouldBe conv1NeighbourExtractorSize
    val result = ConvolutionLayerUnit("convolution", imageInput, hs, ws, hp, wp, hker, wker, hin, win, 1)
    result.output.tFlow shouldBe conv1Output
  }

  it should  "reproduce the expected pattern for CONV2 input" in {
    val (hs,ws, hp,wp,hker,wker,hin,win) = (1,1,0,0,5,5,14,14)
    val wout = (win + 2*wp - wker)/ws + 1
    val hout =  (hin + 2*hp - hker)/hs + 1
    wout shouldBe 10
    hout shouldBe 10
    val size = (hker - 1) * win + wker
    size shouldBe conv2NeighbourExtractorSize
    val result = ConvolutionLayerUnit("convolution", pool1Output, hs, ws, hp, wp, hker, wker, hin, win, 1)
    result.output.tFlow shouldBe conv2Output
  }

  "A pooling layer" should "reproduce the expected pattern for custom example" in {
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val result = PoolingLayerUnit("pooling", customInput, hs, ws, hpool, wpool, hin, win, 1)
    result.output.tFlow shouldBe Stream(14, 24, 26, 28)
  }

  it should "reproduce the expected pattern for POOL1 output" in {
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,28,28)
    val result = PoolingLayerUnit("pooling", conv1Output, hs, ws, hpool, wpool, hin, win, 1)
    result.output.tFlow shouldBe pool1Output
  }

  it should "reproduce the expected pattern for POOL2 output" in {
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,10,10)
    val result = PoolingLayerUnit("pooling", conv2Output, hs, ws, hpool, wpool, hin, win, 1)
    result.output.tFlow shouldBe pool2Output
  }

  "A fully connected layer" should "reproduce the expected pattern for custom example" in {
    val result = DenseChannelUnit(s"FC",customInput)
    result.prodRegister.output.tFlow shouldBe Stream(4, 5, 6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26, 27)
    result.accRegister.outputs(0).tFlow shouldBe Stream(28)
    result.accRegister.outputs(1).tFlow shouldBe Stream(5, 6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26, 27)
    result.output.tFlow shouldBe Stream(29)
  }

  it should "reproduce the expected pattern for FC1 output" in {
    val result = DenseChannelUnit("FC1",pool2Output)
    result.output.tFlow shouldBe fc1Output
  }

  it should "reproduce the expected pattern for FC2 output" in {
    val result = DenseChannelUnit("FC2",seq1Output)
    result.output.tFlow shouldBe fc2Output
  }

  it should "reproduce the expected pattern for FC3 output" in {
    val result = DenseChannelUnit("FC3",seq2Output)
    result.output.tFlow shouldBe fc3Output
  }

  "A sequencer" should "reproduce the expected pattern for custom example" in {
    forAll(
      (Gen.choose(2, 25), "cin/cout"),
      minSuccessful(100)) {
      ratio => {
        val inputTFlow = Stream(1)
        val sequencer = SequencerLayerUnit("sequence",inputTFlow,ratio,1)
        for(i <- sequencer.registers.indices)
          sequencer.registers(i).output.tFlow shouldBe (inputTFlow.head + 1 to inputTFlow.head + i + 1).toStream
      }
    }
  }

  it should "reproduce the expected pattern for SEQ1 output" in {
    val result = SequencerLayerUnit("SEQ1",fc1Output,seq1.inputShape.channel,seq1.outputShape.channel)
    result.output.tFlow shouldBe seq1Output
  }

  it should "reproduce the expected pattern for SEQ2 output" in {
    val result = SequencerLayerUnit("SEQ2",fc2Output,seq2.inputShape.channel,seq2.outputShape.channel)
    result.output.tFlow shouldBe seq2Output
  }
}