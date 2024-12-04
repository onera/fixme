package operators

import models._
import operators.Transformable._
import operators.all._

import scala.language.reflectiveCalls

trait Fallible[T] {
  // counts the number of stuck-at X scenarios for component x
  def stuckAtCount(x: T): Int

  // counts the number of bit-lip scenarios for component x and input flow f
  def bitFlipsCount[U: Streamable](f: TFlow[U], x: T): Int

  // return the stuck-at X scenarios for component x
  def stuckAtScenarios(x: T): Seq[String]

  // return the bit-flips scenarios for component x and input flow f
  def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: T): Seq[String]
}

object Fallible {

  def vulnerableCount[U](f: TFlow[U]): Int = f.size

  def stuckAtXRegister(bitWidth: Int): Int = 2 * bitWidth

  def stuckAtXComb(inputs: Array[Int], outputBitWidth: Int): Int = inputs.sum + outputBitWidth

  def stuckAtXComb(inputCount: Int, inputBitWidth: Int, outputBitWidth: Int): Int = inputCount * inputBitWidth + outputBitWidth

  def stuckAtXComb(inputCount: Int, outputBitWidth: Int): Int = stuckAtXComb(inputCount, outputBitWidth, outputBitWidth)

  def getCombinationalComponentPortsIds(id: String, inputSize: Int): Seq[String] = {
    val inputs = for {
      i <- (0 until inputSize)
    } yield {
      s"${id}.i_${i}"
    }
    inputs :+ s"${id}.o"
  }

  def getNeighbourExtractorSubComponentsPorts(x: Extractor[Int], channels: Int): Seq[String] = {
    for {r <- x.registers
         c <- 0 until channels
         l <- List(r.input.id, r.output.id)
         } yield
      l.replace(x.id, s"${x.id}_$c")
  }

  def getNeighbourExtractorBitFlipsIds[U: Streamable](neighbourExtractor: Extractor[U], channels: Int): Seq[String] = {
    //A BF is created for each register (expansion on cIn ) and each date in the flow
    for {
      cIn <- 0 until channels
      r <- neighbourExtractor.registers
      d <- r.output.tFlow.toDates
    } yield
      s"${r.id.replace(neighbourExtractor.id, s"${neighbourExtractor.id}_$cIn")}@$d"
  }

  trait Instances {
    implicit class FallibleOps[T](x: T)(implicit e: Fallible[T]) {
      def stuckAtCount: Int = e.stuckAtCount(x)

      def bitFlipsCount[U: Streamable](f: TFlow[U]): Int = e.bitFlipsCount(f, x)

      def stuckAtScenarios: Seq[String] = e.stuckAtScenarios(x)

      def bitFlipsScenarios[U: Streamable](f: TFlow[U]): Seq[String] = e.bitFlipsScenarios(f, x)
    }

    implicit val poolingIsFallible: Fallible[Pooling2D] = new Fallible[Pooling2D] {
      val bitWidth = 16

      def stuckAtCount(x: Pooling2D): Int = {
        val registerVerticalExtractor = x.inputShape.width * (x.kernel_size.height - 1) + 1
        val registerHorizontalExtractor = x.kernel_size.width
        val totalRegister = x.inputShape.channel * (registerVerticalExtractor + registerHorizontalExtractor)
        val totalRegisterStuckAtX = totalRegister * stuckAtXRegister(bitWidth)
        val stuckAtMaxVertical = x.inputShape.channel * stuckAtXComb(x.kernel_size.height, bitWidth)
        val stuckAtMaxHorizontal = x.inputShape.channel * stuckAtXComb(x.kernel_size.width, bitWidth)
        totalRegisterStuckAtX + stuckAtMaxVertical + stuckAtMaxHorizontal
      }

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: Pooling2D): Int = {
        val transformed = x.transform(f)
        (transformed.verticalExtractor.registers ++ transformed.horizontalExtractor.registers)
          .map(r => vulnerableCount(r.output.tFlow))
          .sum * bitWidth * x.inputShape.channel
      }


      def stuckAtScenarios(x: Pooling2D): Seq[String] = {
        val t = x.transform(Stream.empty[Int])
        val hExtrSAX = getNeighbourExtractorSubComponentsPorts(t.horizontalExtractor, x.inputShape.channel)
        val vExtrSAX = getNeighbourExtractorSubComponentsPorts(t.verticalExtractor, x.inputShape.channel)
        val vMaxSAX = (0 until x.inputShape.channel).foldLeft(Seq.empty[String])((l, c) =>
          l ++ getCombinationalComponentPortsIds(s"${t.id}.vMax_$c", x.kernel_size.height))
        val hMaxSAX = (0 until x.inputShape.channel).foldLeft(Seq.empty[String])((l, c) =>
          l ++ getCombinationalComponentPortsIds(s"${t.id}.hMax_$c", x.kernel_size.width))
        hExtrSAX ++ vExtrSAX ++ vMaxSAX ++ hMaxSAX
      }


      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: Pooling2D): Seq[String] = {
        val t = x.transform(f)
        val bitflipsVExtr = getNeighbourExtractorBitFlipsIds(t.verticalExtractor, x.inputShape.channel)
        val bitflipsHExtr = getNeighbourExtractorBitFlipsIds(t.horizontalExtractor, x.inputShape.channel)
        bitflipsHExtr ++ bitflipsVExtr
      }

    }

    implicit val convIsFallible: Fallible[Convolution2D] = new Fallible[Convolution2D] {


      def stuckAtCount(x: Convolution2D): Int = {
        val dotBranches = x.kernel_size.height * x.kernel_size.width * x.inputShape.channel
        val reg16Extractor = x.inputShape.channel * ((x.kernel_size.height - 1) * x.inputShape.width + x.kernel_size.width)
        val reg24Prod = x.outputShape.channel * dotBranches
        val reg32Add = x.outputShape.channel
        val totalRegisterStuckAtX =
          reg32Add * stuckAtXRegister(32) +
            reg24Prod * stuckAtXRegister(24) +
            stuckAtXRegister(16) * reg16Extractor
        val stuckAtMax = x.outputShape.channel * stuckAtXComb(1, 32)
        val stuckAtAdd = x.outputShape.channel * stuckAtXComb(dotBranches, 24, 32)
        val stuckAtProd = x.outputShape.channel * dotBranches * stuckAtXComb(1, 16, 24)
        totalRegisterStuckAtX + stuckAtMax + stuckAtAdd + stuckAtProd
      }

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: Convolution2D): Int = {
        val transformed = x.transform(f)
        val reg16BitFlips = x.inputShape.channel * 16 * transformed.neighbourExtractor.registers
          .map(r => vulnerableCount(r.output.tFlow))
          .sum
        val reg32BitFlips = x.outputShape.channel * 32 * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
        val reg24 = x.outputShape.channel * x.kernel_size.height * x.kernel_size.width * x.inputShape.channel
        val reg24BitFlips = reg24 * 24 * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
        reg16BitFlips + reg24BitFlips + reg32BitFlips
      }

      private def getConvolutionChannelUnitSubComponentPorts(x: ConvolutionChannelUnit[Int], convolution2D: Convolution2D) = {
        val dotProductSize = convolution2D.inputShape.channel * convolution2D.kernel_size.height * convolution2D.kernel_size.width
        (0 until convolution2D.outputShape.channel).foldLeft(Seq.empty[String]) {
          (l, c) => {
            val addReg = Seq(x.addRegister.input.id, x.addRegister.output.id)
            val add = getCombinationalComponentPortsIds(s"${x.id}.add", dotProductSize)
            val mults = (0 until dotProductSize)
              .foldLeft(Seq.empty[String]) {
                (seq, i) =>
                  val multReg = Seq(x.prodRegister.input.id, x.prodRegister.output.id).map(_.replace("prodRegister", s"prodRegister_$i"))
                  seq ++ getCombinationalComponentPortsIds(s"${x.id}.prod_${i}", 2) ++ multReg
              }
            val relu = Seq(x.addRegister.input.id, x.addRegister.output.id).map(_.replace("addRegister", s"max"))
            val total = add ++ mults ++ addReg ++ relu
            l ++ total.map(_.replace(s"${x.id}", s"${x.id}_$c"))
          }
        }

      }

      def stuckAtScenarios(x: Convolution2D): Seq[String] = {
        val t = x.transform(Stream.empty[Int])
        val nextrScenarios = getNeighbourExtractorSubComponentsPorts(t.neighbourExtractor, x.inputShape.channel)
        val ccu = getConvolutionChannelUnitSubComponentPorts(t.convolutionChannelUnit, x)
        nextrScenarios ++ ccu
      }

      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: Convolution2D): Seq[String] = {
        val t = x.transform(f)
        val nextrBF = getNeighbourExtractorBitFlipsIds(t.neighbourExtractor, x.inputShape.channel)

        val ccuBFProd = for {
          cOut <- 0 until x.outputShape.channel
          rProdId <- 0 until x.inputShape.channel * x.kernel_size.width * x.kernel_size.height
          d <- t.convolutionChannelUnit.prodRegister.output.tFlow.toDates
        } yield s"${t.convolutionChannelUnit.prodRegister.id.replace(t.convolutionChannelUnit.id, s"${t.convolutionChannelUnit.id}_$cOut")}_$rProdId@$d"
        val ccuBFAdd = for {
          cOut <- 0 until x.outputShape.channel
          d <- t.convolutionChannelUnit.addRegister.output.tFlow.toDates
        } yield s"${t.convolutionChannelUnit.addRegister.id.replace(t.convolutionChannelUnit.id, s"${t.convolutionChannelUnit.id}_$cOut")}@$d"
        nextrBF ++ ccuBFAdd ++ ccuBFProd
      }
    }

    implicit val denseIsFallible: Fallible[Dense] = new Fallible[Dense] {


      def stuckAtCount(x: Dense): Int = {
        val reg16Out = x.outputShape.channel
        val reg24Prod = x.outputShape.channel * x.inputShape.channel
        val reg32Add = x.outputShape.channel
        val totalRegisterStuckAtX =
          stuckAtXRegister(32) * reg32Add +
            stuckAtXRegister(24) * reg24Prod +
            stuckAtXRegister(16) * reg16Out
        val stuckAtAdd = x.outputShape.channel * (
          stuckAtXComb(Array.fill(x.inputShape.channel)(24) :+ 32, 32) + stuckAtXComb(1, 32)
          )
        val stuckAtProd = x.outputShape.channel * x.inputShape.channel * stuckAtXComb(1, 16, 24)
        totalRegisterStuckAtX + stuckAtAdd + stuckAtProd
      }

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: Dense): Int = {
        val transformed = x.transform(f)
        val reg16 = x.outputShape.channel
        val reg24 = x.outputShape.channel * x.inputShape.channel
        val reg32 = x.outputShape.channel
        val reg16BitFlips = reg16 * 16 * vulnerableCount(transformed.outRegister.output.tFlow)
        val reg24BitFlips = reg24 * 24 * vulnerableCount(transformed.prodRegister.output.tFlow)
        val reg32BitFlips = reg32 * 32 * transformed.accRegister.outputs
          .map(r => vulnerableCount(r.tFlow))
          .sum
        reg16BitFlips + reg24BitFlips + reg32BitFlips
      }

      def stuckAtScenarios(x: Dense): Seq[String] = {
        val t = x.transform(Stream.empty[Int])

        val prodPart = for {
          cOut <- 0 until x.outputShape.channel
          cIn <- 0 until x.inputShape.channel
          id <- List(
            t.prodRegister.output.id.replace("prodRegister", s"prodRegister_$cIn"),
            t.prodRegister.output.id.replace("prodRegister", s"prod_$cIn"),
            t.prodRegister.input.id.replace("prodRegister", s"prodRegister_$cIn"),
            t.prodRegister.input.id.replace("prodRegister", s"prod_$cIn"),
            s"${t.id}.ncu_$cOut.add.i_$cIn"
          )
        } yield
          id.replace(t.id, s"${t.id}.ncu_$cOut")
        val accumulatorPart =
          for {
            cOut <- 0 until x.outputShape.channel
            id <- List(
              s"${t.id}.ncu_$cOut.add.o",
              s"${t.id}.ncu_$cOut.add.i_acc",
              t.accRegister.input.id.replace("accRegister", s"ncu_$cOut.accRegister"),
              t.accRegister.resultOut.id.replace("accRegister", s"ncu_$cOut.accRegister"),
              s"${t.id}.ncu_$cOut.biasAdd.i",
              s"${t.id}.ncu_$cOut.biasAdd.o",
              t.outRegister.input.id.replace("outRegister", s"ncu_$cOut.outRegister"),
              t.outRegister.output.id.replace("outRegister", s"ncu_$cOut.outRegister"))
          } yield
            id
        prodPart ++ accumulatorPart
      }

      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: Dense): Seq[String] = {
        val t = x.transform(f)
        //NOTE It is a bit silly to create all the BF in a dense layer knowing that only a few fraction is
        //equivalent, but it is necessary to connect it to the other layers...
        //A BF is created for each register (expansion on cIn and cOut) and each date in the flow
        for {
          cOut <- 0 until x.outputShape.channel
          r <- t.registers
          cIn <- if (r.id.contains("prod")) 0 until x.inputShape.channel else -1 to -1
          o <- r.outputs
          d <- o.tFlow.toDates
        } yield {
          if (r.id.contains("prod")) {
            s"${
              r.id
                .replace(t.id, s"${t.id}.ncu_$cOut")
                .replace("prodRegister", s"prodRegister_$cIn")
            }@$d"
          } else
            s"${r.id.replace(t.id, s"${t.id}.ncu_$cOut")}@$d"
        }
      }
    }

    implicit val sequencerIsFallible: Fallible[Sequencer] = new Fallible[Sequencer] {


      def stuckAtCount(x: Sequencer): Int = x.inputShape.channel * stuckAtXRegister(16)

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: Sequencer): Int = {
        val transformed = x.transform(f)
        16 * x.outputShape.channel * transformed.registers
          .map(r => vulnerableCount(r.output.tFlow))
          .sum
      }

      def stuckAtScenarios(x: Sequencer): Seq[String] = {
        val t = x.transform(Stream.empty[Int])
        val reg = (for {
          cOut <- 0 until x.outputShape.channel
          r <- t.registers
          r_i = r.input.id.replace(".r", s".piso_$cOut.r")
          r_o = r_i.replace(".i", ".o")
        } yield
          Seq(r_i, r_o)).reduce(_ ++ _)

        val mux = (for {
          cOut <- 0 until x.outputShape.channel
          r <- t.registers.init
          mux_i = r.input.id.replace(".r", s".piso_${cOut}.mux")
          mux_o = mux_i.replace(".i", s".o")
        } yield {
          Seq(mux_i + "0", mux_i + "1", mux_o)
        }).reduce(_ ++ _)
        reg ++ mux
      }

      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: Sequencer): Seq[String] = {
        val t = x.transform(f)
        //A BF is created for each register and each date in the flow
        for {
          cOut <- 0 until x.outputShape.channel
          r <- t.registers
          d <- r.output.tFlow.toDates
        } yield
          s"${r.id.replace(".r", s".piso_$cOut.r")}@$d"
      }
    }

    implicit val inputIsNotFallible: Fallible[Input2D] = new Fallible[Input2D] {
      def stuckAtCount(x: Input2D): Int = 0

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: Input2D): Int = 0

      def stuckAtScenarios(x: Input2D): Seq[String] = Seq.empty[String]

      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: Input2D): Seq[String] = Seq.empty[String]
    }

    implicit val layerIsFallible: Fallible[Layer2D] = new Fallible[Layer2D] {
      def stuckAtCount(x: Layer2D): Int = x match {
        case x: Convolution2D => x.stuckAtCount
        case x: Pooling2D => x.stuckAtCount
        case x: Dense => x.stuckAtCount
        case x: Input2D => x.stuckAtCount
        case x: Sequencer => x.stuckAtCount
      }

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: Layer2D): Int = x match {
        case x: Convolution2D => x.bitFlipsCount(f)
        case x: Pooling2D => x.bitFlipsCount(f)
        case x: Dense => x.bitFlipsCount(f)
        case x: Input2D => x.bitFlipsCount(f)
        case x: Sequencer => x.bitFlipsCount(f)
      }

      def stuckAtScenarios(x: Layer2D): Seq[String] = x match {
        case x: Convolution2D => x.stuckAtScenarios
        case x: Pooling2D => x.stuckAtScenarios
        case x: Dense => x.stuckAtScenarios
        case x: Input2D => x.stuckAtScenarios
        case x: Sequencer => x.stuckAtScenarios
      }

      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: Layer2D): Seq[String] = x match {
        case x: Convolution2D => x.bitFlipsScenarios(f)
        case x: Pooling2D => x.bitFlipsScenarios(f)
        case x: Dense => x.bitFlipsScenarios(f)
        case x: Input2D => x.bitFlipsScenarios(f)
        case x: Sequencer => x.bitFlipsScenarios(f)
      }
    }

    implicit def convolutionalNeuralNetworkIsFallible[T <: ConvolutionalNeuralNetwork]: Fallible[T] = new Fallible[T] {

      def stuckAtCount(x: T): Int = x.layers.map(_.stuckAtCount).sum

      def bitFlipsCount[U: Streamable](f: TFlow[U], x: T): Int = {
        val zero = x.input2D.transform(Stream.empty[Int])
        val inputFlows = x.layers.foldLeft(List(zero))((acc, layer) => {
          val outputFlow = layer match {
            case x: Convolution2D => x.transform(acc.last).output.tFlow
            case x: Pooling2D => x.transform(acc.last).output.tFlow
            case x: Dense => x.transform(acc.last).output.tFlow
            case x: Sequencer => x.transform(acc.last).output.tFlow
            case x: Input2D => x.transform(acc.last)
          }
          acc :+ outputFlow
        })
        inputFlows.zip(x.layers).map(p => p._2.bitFlipsCount(p._1)).sum
      }

      def stuckAtScenarios(x: T): Seq[String] = {
        x.layers.foldLeft(Seq.empty[String]) { (acc, l) => acc ++ l.stuckAtScenarios }
      }

      def bitFlipsScenarios[U: Streamable](f: TFlow[U], x: T): Seq[String] = {

        val inputFlows = x.layers.foldLeft(List(f))((acc, layer) => {
          val outputFlow = layer match {
            case x: Convolution2D => x.transform(acc.last).output.tFlow
            case x: Pooling2D => x.transform(acc.last).output.tFlow
            case x: Dense => x.transform(acc.last).output.tFlow
            case x: Sequencer => x.transform(acc.last).output.tFlow
            case x: Input2D => x.transform(acc.last)
          }
          acc :+ outputFlow
        })
        inputFlows.zip(x.layers).map(p => p._2.bitFlipsScenarios(p._1)).reduce(_ ++ _)
      }
    }
  }
}
