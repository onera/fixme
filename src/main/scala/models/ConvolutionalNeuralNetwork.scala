package models

sealed abstract class Layer[T] {
  val name: String
  val inputShape: T
  val outputShape: T
}

case class Shape2D(height: Int, width: Int, channel: Int = 1)

sealed abstract class Layer2D extends Layer[Shape2D]

case class Convolution2D(name: String, inputShape: Shape2D, strides:Shape2D, kernel_size:Shape2D, valid_padding:Boolean, filters:Int) extends Layer2D {
  val hOut: Int =
    if(valid_padding)
      Math.floor((inputShape.height - (kernel_size.height - 1) - 1).toDouble / strides.height + 1).toInt
    else
      inputShape.height

  val wOut: Int =
    if(valid_padding)
      Math.floor((inputShape.width - (kernel_size.width - 1) - 1).toDouble / strides.width + 1).toInt
    else
      inputShape.width

  val padding_size: Shape2D =
    if(valid_padding)
      Shape2D(0,0,0)
    else
      Shape2D(
        (inputShape.height * (strides.height - 1) - strides.height + kernel_size.height)/2,
        (inputShape.width * (strides.width - 1) - strides.width + kernel_size.width)/2,
        0)

  val outputShape: Shape2D = Shape2D(hOut,wOut,filters)
}

case class Pooling2D(name: String, inputShape: Shape2D, strides:Shape2D, kernel_size:Shape2D, valid_padding:Boolean) extends Layer2D {
  val hOut: Int =
    if(valid_padding)
      Math.floor((inputShape.height - (kernel_size.height - 1) - 1).toDouble / strides.height + 1).toInt
    else
      inputShape.height

  val wOut: Int =
    if(valid_padding)
      Math.floor((inputShape.width - (kernel_size.width - 1) - 1).toDouble / strides.width + 1).toInt
    else
      inputShape.width

  val outputShape: Shape2D = Shape2D(hOut,wOut,inputShape.channel)
}

case class Dense(name: String, inputShape: Shape2D, outputShape: Shape2D) extends Layer2D {
  def sequential(targetChannelSize: Int): Option[Sequencer] = {
    if (outputShape.channel % targetChannelSize == 0) {
      Some(Sequencer(s"${name}_seq", outputShape, Shape2D(outputShape.channel / targetChannelSize, 1, targetChannelSize)))
    } else None
  }
}

case class Input2D(name: String, inputShape: Shape2D, outputShape: Shape2D) extends Layer2D

case class Sequencer(name: String, inputShape: Shape2D, outputShape: Shape2D) extends Layer2D

case class ConvolutionalNeuralNetwork(input2D: Input2D, layers: Seq[Layer2D], name: String) {
  def sequential(index: Int, targetChannelSize: Int): ConvolutionalNeuralNetwork = {
    layers(index) match {
      case l: Dense =>
        val (previousLayers, nextLayers) = layers.splitAt(index + 1)
        (for {seqLayer <- l.sequential(targetChannelSize)}
          yield copy(layers = (previousLayers :+ seqLayer) ++ nextLayers)).getOrElse(this)

      case x =>
        println(s"Error cannot sequentialize $x")
        this
    }
  }

  def getLayerByName(name: String): Option[Layer2D] = {
    layers.find(_.name.matches(name))
  }

  def sequential(name: String, targetChannelSize: Int): ConvolutionalNeuralNetwork = {
    sequential(layers.indexWhere(_.name == name), targetChannelSize)
  }

  def remove(index: Int): ConvolutionalNeuralNetwork = {
    copy(layers = layers.indices.filter(_ != index).map(x => layers(x)))
  }

  def remove(name: String): ConvolutionalNeuralNetwork = {
    copy(layers = layers.filter(_.name != name))
  }

  override def toString: String = name

  def appendLeft(layer2D: Layer2D): ConvolutionalNeuralNetwork = {
    copy(layers = layer2D +: layers)
  }
}
