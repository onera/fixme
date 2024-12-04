package parsers

import io.circe._
import io.circe.parser.{parse => parseJson}
import models._

import java.io.File
import scala.io.Source

object CNNJsonParser {

  /**
   * Transform an array of Int/null JSON element to a Shape2D considering that
   * should contains two elements that stands for width::height
   *
   * @param l the list of Json element
   * @return the resulting Shape2D
   */
  def decodeShape2D(l: List[Json]): Either[DecodingFailure, Shape2D] = {
    val dim = for {b <- l
                   bInt <- b.hcursor.as[Int].toOption} yield bInt
    dim match {
      case width :: height :: Nil => Right(Shape2D(height, width, 0))
      case _ => Left(DecodingFailure("Cannot parse Shape2D", Nil))
    }
  }

  /**
   * Trait for all case classes capturing the JSON config of the layers
   */
  trait Config{
    val name:String
    val dtype:String
  }

  /**
   * Config for the input layer
   * @param name the name of the layer
   * @param dtype the data type
   * @param sparse is it sparse layer
   * @param ragged is ragged
   * @param batch_input_shape the shape of the inputs
   */
  case class InputLayerConfig (name:String, dtype:String, sparse:Boolean, ragged:Boolean, batch_input_shape:Shape2D) extends Config

  /**
   * implicit decoder from JSON to InputLayerConfig
   */
  implicit val decoderInputLayerConfig: Decoder[InputLayerConfig] = (hCursor:HCursor) =>
    for {
      name <- hCursor.get[String]("name")
      dtype <- hCursor.get[String]("dtype")
      spare <- hCursor.downField("sparse").as[Boolean]
      ragged <- hCursor.downField("ragged").as[Boolean]
      batch_input_shape <- hCursor.downField("batch_input_shape").as[List[Json]]
      shape <- decodeShape2D(batch_input_shape)
    } yield {
      InputLayerConfig(name,dtype,spare, ragged, shape)
    }

  /**
   * Config for the convolution layer
   * @param name the name of the layer
   * @param dtype the data type
   * @param filters the number of channels
   * @param kernel_size the dimensions of the kernel
   * @param strides the 2D stride
   * @param validPadding is it a VALID padding (false stands for SAME padding)
   */
  case class ConvLayerConfig (name:String, dtype:String, filters:Int, kernel_size:Shape2D, strides:Shape2D, validPadding:Boolean) extends Config

  /**
   * implicit decoder from JSON to ConvLayerConfig
   */
  implicit val decoderConvLayerConfig: Decoder[ConvLayerConfig] = (hCursor:HCursor) =>
    for {
      name <- hCursor.get[String]("name")
      dtype <- hCursor.get[String]("dtype")
      filters <- hCursor.downField("filters").as[Int]
      padding <- hCursor.get[String]("padding")
      kernel_size <- hCursor.downField("kernel_size").as[List[Json]]
      strides <- hCursor.downField("strides").as[List[Json]]
      kernelShape <- decodeShape2D(kernel_size)
      strideShape <- decodeShape2D(strides)
      validPadding = padding == "valid"
    } yield {
      ConvLayerConfig(name,dtype,filters, kernelShape, strideShape,validPadding)
    }

  /**
   * Config for the pool layer
   * @param name the name of the layer
   * @param dtype the data type
   * @param pool_size the dimension of the pool kernel
   * @param strides the 2D stride
   * @param validPadding is it a VALID padding (false stands for SAME padding)
   */
  case class PoolLayerConfig (name:String, dtype:String, pool_size:Shape2D, strides:Shape2D, validPadding:Boolean) extends Config

  /**
   * implicit decoder from JSON to PoolLayerConfig
   */
  implicit val decoderPoolLayerConfig: Decoder[PoolLayerConfig] = (hCursor:HCursor) =>
    for {
      name <- hCursor.get[String]("name")
      dtype <- hCursor.get[String]("dtype")
      padding <- hCursor.get[String]("padding")
      pool_size <- hCursor.downField("pool_size").as[List[Json]]
      strides <- hCursor.downField("strides").as[List[Json]]
      poolShape <- decodeShape2D(pool_size)
      strideShape <- decodeShape2D(strides)
      validPadding = padding == "valid"
    } yield {
      PoolLayerConfig(name,dtype, poolShape, strideShape,validPadding)
    }

  /**
   * Config for the dense layer
   * @param name the name of the layer
   * @param dtype the data type
   * @param units the number of neurons
   */
  case class DenseConfig(name:String, dtype:String, units:Int) extends Config

  /**
   * implicit decoder from JSON to DenseConfig
   */
  implicit val decoderDensLayerConfig: Decoder[DenseConfig] = (hCursor:HCursor) =>
    for {
      name <- hCursor.get[String]("name")
      dtype <- hCursor.get[String]("dtype")
      units <- hCursor.downField("units").as[Int]
    } yield {
      DenseConfig(name,dtype, units)
    }

  /**
   * Transform the JSON element representing a layer to the proper configuration
   * Currently only input, convolution, max pool and dense layers are supported
   * @param layer the JSON element representing the layer
   * @return the optional configuration (can fail if the layer is not supported or the element
   *         do not encode a layer)
   */
  def decodeLayer(layer:Json): Option[Config] =
    (for {
      name <- layer.hcursor.downField("class_name").as[String].toOption
      if name == "InputLayer" || name == "Conv2D" || name == "MaxPooling2D" || name == "Dense"
      config <- layer.hcursor.downField("config").focus
    }
      yield name match {
        case "InputLayer" =>
          config.as[InputLayerConfig].toOption
        case "Conv2D" =>
          config.as[ConvLayerConfig].toOption
        case "MaxPooling2D" =>
          config.as[PoolLayerConfig].toOption
        case "Dense" =>
          config.as[DenseConfig].toOption
      }).flatten

  /**
   * Transform the configuration into shape specified layers.
   * The computation of the output shape of a layer depends on the layer type.
   * If the layer is a convolution or a pooling and use SAME padding then the output shape is the same
   * as the input shape, if a VALID padding is used then the computation of the new dimensions is performed
   * according to the formula from https://pytorch.org/docs/stable/generated/torch.nn.Conv2d.html.
   * If the layer is an input layer then the output shape is the same as the input shape.
   * If the layer is a dense layer then its ouput shape is (1,1,units) where units is the number of neurons in the layer
   *
   * @param configs the ordered list of the layers' config
   * @return the indexed map of the shape oriented layer description
   */
  def transformConfigToLayer(configs: List[Config]): List[Layer2D] = {
    configs.zipWithIndex.foldLeft(List.empty[Layer2D])((acc, p) => p match {
      case (InputLayerConfig(name, _, _, _, batch_input_shape), _) =>
        acc :+ Input2D(name, batch_input_shape.copy(channel = 1), batch_input_shape.copy(channel = 1))
      case (ConvLayerConfig(name, _, filters, kernel_size, strides, isValidPadding), i) =>
        acc :+ Convolution2D(name, acc(i - 1).outputShape, strides,kernel_size,isValidPadding,filters)
      case (PoolLayerConfig(name, _, pool_size, strides, isValidPadding),i) =>
        acc :+ Pooling2D(name, acc(i - 1).outputShape, strides,pool_size,isValidPadding)
      case (DenseConfig(name, _, units),i) =>
        acc :+ Dense(name, acc(i - 1).outputShape, Shape2D(1, 1, units))
    })
  }

  /**
   * Parse a JSON export of a Keras model
   *
   * @param modelFile the JSON file
   * @return the indexed map of the shape oriented layer description
   */
  def parseModel(modelFile: File): Option[ConvolutionalNeuralNetwork] = {
    val s = Source.fromFile(modelFile)
    val json = s.getLines().mkString("\n")
    s.close()
    (for {
      doc <- parseJson(json)
      name <- doc.hcursor.downField("name").as[String]
      layers <- doc.hcursor.downField("layers").as[List[Json]]
    } yield {
      val configs = for {
        layer <- layers
        decoded <- decodeLayer(layer)
      } yield {
        decoded
      }
      val transformed = transformConfigToLayer(configs)
      val inputs = transformed.collect({case x:Input2D => x})
      val others = transformed.filterNot(inputs.contains)
      val input = if(inputs.nonEmpty) inputs.head else Input2D(s"input",others.head.inputShape,others.head.inputShape)
      ConvolutionalNeuralNetwork(input, others, name)
    }).toOption
  }
}
