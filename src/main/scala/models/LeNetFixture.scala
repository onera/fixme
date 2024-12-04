package models

import operators.Transformable._
import operators.all._

object LeNetFixture {

  private val LeNetConv1 = Convolution2D("CONV1", Shape2D(32, 32), Shape2D(1, 1), Shape2D(5, 5), valid_padding = true, 6)
  private val LeNetPool1 = Pooling2D("POOL1", Shape2D(28, 28, 6), Shape2D(2, 2), Shape2D(2, 2), valid_padding = true)
  private val LeNetConv2 = Convolution2D("CONV2", Shape2D(14, 14, 6), Shape2D(1, 1), Shape2D(5, 5), valid_padding = true, 16)
  private val LeNetPool2 = Pooling2D("POOL2", Shape2D(10, 10, 16), Shape2D(2, 2), Shape2D(2, 2), valid_padding = true)
  private val LeNetFC1 = Dense("FC1", Shape2D(5, 5, 16), Shape2D(1, 1, 120))
  private val LeNetSEQ1 = Sequencer("SEQ1", Shape2D(1, 1, 120), Shape2D(12, 1, 10))
  private val LeNetFC2 = Dense("FC2", Shape2D(12, 1, 10), Shape2D(1, 1, 84))
  private val LeNetSEQ2 = Sequencer("SEQ2", Shape2D(1, 1, 84), Shape2D(4, 1, 21))
  private val LeNetFC3 = Dense("FC3", Shape2D(4, 1, 21), Shape2D(1, 1, 10))

  def mkSimpleLeNet(input2D: Input2D): ConvolutionalNeuralNetwork = {
    val conv1 = Convolution2D("CONV1", input2D.outputShape, Shape2D(1, 1), Shape2D(5, 5), valid_padding = true, 1)
    val pool1 = Pooling2D("POOL1", conv1.outputShape, Shape2D(2, 2), Shape2D(2, 2), valid_padding = true)
    val conv2 = Convolution2D("CONV2", pool1.outputShape, Shape2D(1, 1), Shape2D(5, 5), valid_padding = true, 1)
    val pool2 = Pooling2D("POOL2", conv2.outputShape, Shape2D(2, 2), Shape2D(2, 2), valid_padding = true)
    val fc1 = Dense("FC1", pool2.outputShape, Shape2D(1, 1, 12))
    val seq1 = Sequencer("SEQ1", fc1.outputShape, Shape2D(6, 1, 2))
    val fc2 = Dense("FC2", seq1.outputShape, Shape2D(1, 1, 8))
    val seq2 = Sequencer("SEQ2", fc2.outputShape, Shape2D(4, 1, 2))
    val fc3 = Dense("FC3", seq2.outputShape, Shape2D(1, 1, 2))
    ConvolutionalNeuralNetwork(input2D, layers = Seq(conv1, pool1, conv2, pool2, fc1, seq1, fc2, seq2, fc3), "LeNetLike")
  }

  def mkLeNetInputTFlow(input2D: Input2D): TFlow[Int] = {
    lazy val imageInput: TFlow[Int] =
      (0 #:: imageInput.map(n => n + 1)) take input2D.inputShape.height * LeNetFixtureSafecompPaper.input2D.inputShape.width

    imageInput
  }

  abstract class LeNetFixture extends ConvolutionalNeuralNetwork(Input2D("input", Shape2D(32, 32), Shape2D(32, 32)),
    Seq(LeNetConv1, LeNetPool1, LeNetConv2, LeNetPool2, LeNetFC1, LeNetSEQ1, LeNetFC2, LeNetSEQ2, LeNetFC3),
    "LeNet5") {
    val conv1: Convolution2D = LeNetConv1
    val pool1: Pooling2D = LeNetPool1
    val conv2: Convolution2D = LeNetConv2
    val pool2: Pooling2D = LeNetPool2
    val fc1: Dense = LeNetFC1
    val seq1: Sequencer = LeNetSEQ1
    val fc2: Dense = LeNetFC2
    val seq2: Sequencer = LeNetSEQ2
    val fc3: Dense = LeNetFC3

    val conv1NeighbourExtractorSize: Int = 133

    val conv2NeighbourExtractorSize: Int = 61

    val pool1SizeVerticalExtractor: Int = 29

    val pool1SizeHorizontalExtractor: Int = 2

    val pool2SizeVerticalExtractor: Int = 11

    val pool2SizeHorizontalExtractor: Int = 2

    val customInput: Stream[Int] =
      Stream(1, 4, 5, 6, 9, 10, 13, 14, 15, 17, 19, 20, 23, 24, 25, 26)

    val customElement: Stream[Element] =
      customInput.indices.map(i => Element(customInput(i), i)).toStream

    val customCorruptibleElement: Stream[CorruptibleElement] =
      customInput.indices.map(i => CorruptibleElement(customInput(i), i)).toStream

    val imageInput: TFlow[Int]

    val imageInputElement: TFlow[Element]

    val conv1NeighbourExtractorOutput: TFlow[Int]

    val conv1Output: TFlow[Int]

    val pool1Output: TFlow[Int]

    val conv2NeighbourExtractorOutput: TFlow[Int]

    val conv2Output: TFlow[Int]

    val pool2Output: TFlow[Int]

    val fc1Output: TFlow[Int]
    val seq1Output: TFlow[Int]

    val fc2Output: TFlow[Int]
    val seq2Output: TFlow[Int]

    val fc3Output: TFlow[Int]
  }

  object LeNetFixtureSafecompPaper extends LeNetFixture {

    lazy val imageInput: TFlow[Int] =
      (0 #:: imageInput.map(n => n + 1)) take this.input2D.inputShape.height * this.input2D.inputShape.width

    lazy val imageInputElement: TFlow[Element] =
      imageInput.indices.map(i => Element(imageInput(i), i)).toStream

    lazy val conv1NeighbourExtractorOutput: TFlow[Int] =
      imageInput.zipWithIndex.filter(p => {
        val (_, n) = p
        n >= this.conv1NeighbourExtractorSize &&
          ((n - this.conv1NeighbourExtractorSize) % this.input2D.inputShape.width - this.conv1.outputShape.width < 0 ||
            (n - this.conv1NeighbourExtractorSize) % this.input2D.inputShape.width - this.conv1.outputShape.width >= this.conv1.kernel_size.width)
      }).map(_._1) ++ Stream(imageInput.last + 1)

    lazy val conv1Output: TFlow[Int] = conv1NeighbourExtractorOutput.shift(2)

    lazy val pool1Output: TFlow[Int] =
      conv1Output.zipWithIndex.filter(p => {
        val (_, n) = p
        n >= this.pool1SizeHorizontalExtractor + this.pool1SizeVerticalExtractor &&
          (n % this.pool1.inputShape.width) % this.pool1.strides.width == 1 &&
          (n / this.pool1.inputShape.width) % this.pool1.strides.height == 1
      }).map(_._1) ++ Stream(conv1Output.last + this.pool1.strides.width)

    lazy val conv2NeighbourExtractorOutput: TFlow[Int] =
      pool1Output.zipWithIndex.filter(p => {
        val (_, n) = p
        n >= this.conv2NeighbourExtractorSize &&
          ((n - this.conv2NeighbourExtractorSize) % this.conv2.inputShape.width - this.conv2.outputShape.width < 0 ||
            (n - this.conv2NeighbourExtractorSize) % this.conv2.inputShape.width - this.conv2.outputShape.width >= this.conv2.kernel_size.width)
      }).map(_._1) ++ Stream(pool1Output.last + 1)

    lazy val conv2Output: TFlow[Int] = conv2NeighbourExtractorOutput.shift(2)

    lazy val pool2Output: TFlow[Int] =
      conv2Output.zipWithIndex.filter(p => {
        val (_, n) = p
        n >= this.pool2SizeHorizontalExtractor + this.pool2SizeVerticalExtractor &&
          (n % this.pool2.inputShape.width) % this.pool2.strides.width == 1 &&
          (n / this.pool2.inputShape.width) % this.pool2.strides.height == 1
      }).map(_._1) ++ Stream(conv2Output.last + this.pool2.strides.width)

    lazy val fc1Output: TFlow[Int] = Stream(pool2Output.last).shift(3)
    lazy val seq1Output: TFlow[Int] = (fc1Output.head + 1 to fc1Output.head + seq1.inputShape.channel / seq1.outputShape.channel).toStream

    lazy val fc2Output: TFlow[Int] = Stream(seq1Output.last).shift(3)
    lazy val seq2Output: TFlow[Int] = (fc2Output.head + 1 to fc2Output.head + seq2.inputShape.channel / seq2.outputShape.channel).toStream

    lazy val fc3Output: TFlow[Int] = Stream(seq2Output.last).shift(3)
  }

  /**
   * This is a modification of LeNetFixture that fits the architecture implemented on FPGA
   */
  object VhdlImplementedLeNetFixture extends LeNetFixture {

    lazy val imageInput: TFlow[Int] =
      (1 #:: imageInput.map(n => n + 1)) take this.input2D.inputShape.height * this.input2D.inputShape.width

    lazy val imageInputElement: TFlow[Element] =
      imageInput.indices.map(i => Element(imageInput(i), i)).toStream

    lazy val conv1NeighbourExtractorOutput: TFlow[Int] =
      imageInput.zipWithIndex.filter(p => {
        val (_, n) = p
        val lineCount = n / conv1.inputShape.width
        val colCount = n % conv1.inputShape.width

        lineCount >= this.conv1.kernel_size.height - 1 &&
          lineCount < conv1.inputShape.width &&
          colCount >= conv1.kernel_size.width - 1 &&
          (conv1.inputShape.width * lineCount + colCount) % conv1.strides.width >= 0
      }).map(_._1 + 2) //++ Stream(imageInput.last + 1)

    lazy val conv1Output: TFlow[Int] = conv1NeighbourExtractorOutput.map(p => p + 2) //.shift(2)

    lazy val pool1Output: TFlow[Int] = pool1HOutput

    lazy val pool1VOutput: TFlow[Int] = conv1Output.zipWithIndex.filter(p => {
      val (_, n) = p
      n >= this.pool1SizeVerticalExtractor - 1 &&
        (n / this.pool1.inputShape.width) % this.pool1.strides.height == 1
    }).map(_._1 + 2) //++ Stream(conv1Output.last + this.pool1.strides.width)

    lazy val pool1HOutput: TFlow[Int] = pool1VOutput.zipWithIndex.filter(p => {
      val (_, n) = p
      n >= this.pool1SizeHorizontalExtractor - 1 &&
        (n % this.pool1.inputShape.width) % this.pool1.strides.width == 1
    }).map(_._1 + 2)

    lazy val conv2NeighbourExtractorOutput: TFlow[Int] =
      pool1Output.zipWithIndex.filter(p => {
        val (_, n) = p
        val lineCount = n / conv2.inputShape.width
        val colCount = n % conv2.inputShape.width

        lineCount >= this.conv2.kernel_size.height - 1 &&
          lineCount < conv2.inputShape.width &&
          colCount >= conv2.kernel_size.width - 1 &&
          (conv2.inputShape.width * lineCount + colCount) % conv2.strides.width >= 0
      }).map(_._1 + 2) //++ Stream(imageInput.last + 1)

    lazy val conv2Output: TFlow[Int] = conv2NeighbourExtractorOutput.map(_ + 2)

    lazy val pool2Output: TFlow[Int] = pool2HOutput

    lazy val pool2VOutput: TFlow[Int] = conv2Output.zipWithIndex.filter(p => {
      val (_, n) = p
      n >= this.pool2SizeVerticalExtractor - 1 &&
        (n / this.pool2.inputShape.width) % this.pool2.strides.height == 1
    }).map(_._1 + 2)

    lazy val pool2HOutput: TFlow[Int] = pool2VOutput.zipWithIndex.filter(p => {
      val (_, n) = p
      n >= this.pool2SizeHorizontalExtractor - 1 &&
        (n % this.pool2.inputShape.width) % this.pool2.strides.width == 1
    }).map(_._1 + 2)

    lazy val fc1Output: TFlow[Int] = Stream(pool2Output.last).shift(3)
    lazy val seq1Output: TFlow[Int] = (fc1Output.head + 1 to fc1Output.head + seq1.inputShape.channel / seq1.outputShape.channel).toStream.map(_ + 1)

    lazy val fc2Output: TFlow[Int] = Stream(seq1Output.last).shift(3)
    lazy val seq2Output: TFlow[Int] = (fc2Output.head + 1 to fc2Output.head + seq2.inputShape.channel / seq2.outputShape.channel).toStream.map(_ + 1)

    lazy val fc3Output: TFlow[Int] = Stream(seq2Output.last).shift(3)
  }

}
