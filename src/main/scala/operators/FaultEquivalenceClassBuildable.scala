package operators

import models._
import operators.Transformable.{ConvolutionLayerUnit, Extractor, TFlow}
import operators.all._
import utils.UnionFind

trait FaultEquivalenceClassBuildable[T] {
  def stuckAtClasses(x: T): UnionFind[String]

  def bitFlipClasses[U: Streamable](f: TFlow[U], x: T): UnionFind[String]

}

object FaultEquivalenceClassBuildable {

  private def applyBitFlipsEquivalenceRuleOnExtractor[U: Streamable]( extr: Extractor[U],result: UnionFind[String],winShape: Shape2D,inputShape: Shape2D) = {
    import result._
    // For extractorRegisters outside of the extracting window, the BF are equivalent
    val windowIndices = for {k <- 0 until winShape.width; l <- 0 until winShape.height} yield k + l * inputShape.width
    for {
      cIn <- 0 until inputShape.channel
      rIdx <- extr.registers.indices if !windowIndices.contains(rIdx)
      (d_i, date_i) <- extr.registers(rIdx).output.tFlow.indices.zip(extr.registers(rIdx).output.tFlow.toDates)
      bf = s"${extr.registers(rIdx).id.replace(extr.id, s"${extr.id}_$cIn")}@$date_i"
    } {
      // if the TODO
      if (rIdx < extr.registers.length - 1 &&
        extr.registers(rIdx + 1).output.tFlow.size >= d_i + 1) {
        val dateNext = extr.registers(rIdx + 1).output.tFlow.toDates(d_i)
        bf <=> s"${extr.registers(rIdx + 1).id.replace(extr.id, s"${extr.id}_$cIn")}@$dateNext"
      }

    }

    // For the extractorRegisters in the extracting window, the BF is equivalent iff the erroneous data is used only by the next register
    for {
      cIn <- 0 until inputShape.channel
      rIdx <- windowIndices
      (d_i, date_i) <- extr.registers(rIdx).output.tFlow.indices.zip(extr.registers(rIdx).output.tFlow.toDates)
      // if the date is not in the output flow of the extractor, there may exist an equivalent BF in the next register
      if !extr.output.tFlow.toDates.contains(date_i)
      bf = s"${extr.registers(rIdx).id.replace(extr.id, s"${extr.id}_$cIn")}@$date_i"
    } {
      if (rIdx < extr.registers.length - 1 &&
        extr.registers(rIdx + 1).output.tFlow.size > d_i + 1) {
        val dateNext = extr.registers(rIdx + 1).output.tFlow.toDates(d_i)
        bf <=> s"${extr.registers(rIdx + 1).id.replace(extr.id, s"${extr.id}_$cIn")}@$dateNext"
      }
    }
  }

  trait Instances {
    implicit class FaultEquivalenceClassBuildableOps[T](x: T)(implicit e: FaultEquivalenceClassBuildable[T]) {

      def stuckAtXClasses: UnionFind[String] = e.stuckAtClasses(x)

      def bitFlipClasses[U: Streamable](f: TFlow[U]): UnionFind[String] = e.bitFlipClasses(f, x)
    }

    implicit val poolingIsFaultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[Pooling2D] = new FaultEquivalenceClassBuildable[Pooling2D] {
      val bitWidth = 16


      def bitFlipClasses[U: Streamable](f: TFlow[U], x: Pooling2D): UnionFind[String] = {
        val t = x.transform(f)

        val result = UnionFind.empty[String]
        result ++= x.bitFlipsScenarios(f)

        //An abstract BF is created for each I/O flow just to allow equivalence propagation among layers
        for {
          cOut <- 0 until x.outputShape.channel
        } {
          for {
            d <- t.input.tFlow.toDates
          }
            result += s"${t.input.id}_$cOut@$d"

          for {
            d <- t.output.tFlow.toDates
          }
            result += s"${t.output.id}_$cOut@$d"
        }

        import result._
        // Input abstract BFs are equivalent to first vertical extractor BFs
        val vExtr = t.verticalExtractor
        for {
          cIn <- 0 until x.inputShape.channel
          (d_i, d_r) <- t.input.tFlow.toDates.zip(t.registers.head.output.tFlow.toDates)
        } yield {
          s"${t.input.id}_$cIn@$d_i" <=> s"${t.registers.head.id.replace(vExtr.id, s"${vExtr.id}_$cIn")}@$d_r"
        }
        val hExtr = t.horizontalExtractor
        applyBitFlipsEquivalenceRuleOnExtractor(vExtr, result, x.kernel_size.copy(width = 1), x.inputShape)
        applyBitFlipsEquivalenceRuleOnExtractor(hExtr, result, x.kernel_size.copy(width = 1), x.inputShape.copy(width = 1))
        // For extractorRegisters outside of the extracting window, the BF are equivalent
        result
      }


      def stuckAtClasses(x: Pooling2D): UnionFind[String] = {
        val t = x.transform(Stream.empty[Int])
        val result = UnionFind.empty[String]
        result ++= x.stuckAtScenarios
        for {
          c <- 0 until x.inputShape.channel
          f <- List(t.input, t.output)
        }
          result += s"${f.id}_$c"

        import result._
        //ENCODING RULE ONE ON VERTICAL MAX CONNECTION TO FIRST HORIZONTAL EXTRACTOR REGISTER
        for {
          c <- 0 until x.inputShape.channel
          vMax_o = t.verticalMax.outputs.head.id.replace("vMax", s"vMax_$c")
          hMax_o = t.horizontalMax.outputs.head.id.replace("hMax", s"hMax_$c")
          hR0_i = t.horizontalExtractor.registers.head.input.id.replace("Extractor", s"Extractor_$c")
          input = s"${t.input.id}_$c"
          output = s"${t.output.id}_$c"
          vR0_i = t.verticalExtractor.registers.head.input.id.replace("Extractor", s"Extractor_$c")
        } {
          vMax_o <=> hR0_i
          input <=> vR0_i
          hMax_o <=> output
          for {
            i <- t.verticalExtractor.registers.indices.init if i % x.inputShape.width != 0
            rI_o = t.verticalExtractor.registers(i).output.id.replace("Extractor", s"Extractor_$c")
            rIP1_i = t.verticalExtractor.registers(i + 1).input.id.replace("Extractor", s"Extractor_$c")
            rIP1_o = t.verticalExtractor.registers(i + 1).output.id.replace("Extractor", s"Extractor_$c")
          } {
            rI_o <=> rIP1_i
            if ((i + 1) % x.inputShape.width != 0)
              rIP1_i <=> rIP1_o
          }
        }
        result
      }


    }

    implicit val convIsFaultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[Convolution2D] = new FaultEquivalenceClassBuildable[Convolution2D] {

      def bitFlipClasses[U: Streamable](f: TFlow[U], x: Convolution2D): UnionFind[String] = {
        val t = x.transform(f)

        val result = UnionFind.empty[String]


        result ++= x.bitFlipsScenarios(f)

        //An abstract BF is created for each I/O flow just to allow equivalence propagation among layers
        for {
          cIn <- 0 until x.inputShape.channel
          d <- t.input.tFlow.toDates
        }
          result += s"${t.input.id}_$cIn@$d"

        for {
          cOut <- 0 until x.outputShape.channel
          d <- t.output.tFlow.toDates
        }
          result += s"${t.output.id}_$cOut@$d"



        // only extractors contains equivalences
        applyBitFlipsEquivalenceRuleOnExtractor(t.neighbourExtractor, result,x.kernel_size,x.inputShape)

        import result._
        // Input abstract BFs are equivalent to first register NE BFs
        for {
          cIn <- 0 until x.inputShape.channel
          (d_i, d_r) <- t.input.tFlow.toDates.zip(t.neighbourExtractor.registers.head.output.tFlow.toDates)
        } yield
          s"${t.input.id}_$cIn@$d_i" <=> s"${t.registers.head.id.replace(t.neighbourExtractor.id, s"${t.neighbourExtractor.id}_$cIn")}@$d_r"

        result
      }


      def applyRule1EquivalenceDotProduct(unionFind: UnionFind[String], convolutionLayerUnit: ConvolutionLayerUnit[Int], convolution2d: Convolution2D) = {
        val x = convolutionLayerUnit.convolutionChannelUnit
        import unionFind._

        //ENCODING RULE 1
        for {cOut <- 0 until convolution2d.outputShape.channel
             max_o = s"${x.id}_$cOut.max.o"
             max_i = s"${x.id}_$cOut.max.i"
             output = s"${convolutionLayerUnit.output.id}_$cOut"
             addReg_i = x.addRegister.input.id.replace("dotProduct", s"dotProduct_$cOut")
             addReg_o = x.addRegister.output.id.replace("dotProduct", s"dotProduct_$cOut")
             add_o = s"${x.id}_$cOut.add.o"
             } {
          for {
            cIn <- 0 until convolution2d.kernel_size.width * convolution2d.kernel_size.height * convolution2d.inputShape.channel
            add_i = s"${x.id}_$cOut.add.i_$cIn"
            prodReg_o = s"${x.id}_$cOut.prodRegister_$cIn.o"
            prodReg_i = s"${x.id}_$cOut.prodRegister_$cIn.i"
            prod_o = s"${x.id}_$cOut.prod_$cIn.o"
          } {
            prod_o <=> prodReg_i
            prodReg_i <=> prodReg_o
            prodReg_o <=> add_i
          }
          add_o <=> addReg_i
          addReg_i <=> addReg_o
          addReg_o <=> max_i
          //            max_i <=> max_o //TRUE ONLY FOR THE 31 BITS -- EDIT: THIS WORKS ONLY FOR SA0
          max_o <=> output
        }
        unionFind
      }

      private def applyRule1NExtr(result: UnionFind[String], x: Convolution2D, t: ConvolutionLayerUnit[Int]) = {
        import result._
        // stuck-at on the input of the CLU are equivalent to stuck-at on the first register input
        for {
          cIn <- 0 until x.inputShape.channel
          input = s"${t.input.id}_$cIn"
          r0_i = t.neighbourExtractor.registers.head.input.id.replace(t.neighbourExtractor.id, s"${t.neighbourExtractor.id}_$cIn")
        }
          input <=> r0_i

        // stuck-at on the inputs of registers are equivalen
        for {
          cIn <- 0 until x.inputShape.channel
          i <- t.neighbourExtractor.registers.indices
          ri_o = t.neighbourExtractor.registers(i).output.id.replace(t.neighbourExtractor.id, s"${t.neighbourExtractor.id}_$cIn")
          ri_i = t.neighbourExtractor.registers(i).input.id.replace(t.neighbourExtractor.id, s"${t.neighbourExtractor.id}_$cIn")

        } {
          ri_i <=> ri_o
        }
        // stuck-at on the output of registers outside of the window are equivalent to stuck-at on the input of the next register
        val windowIndices = for {k <- 0 until x.kernel_size.width; l <- 0 until x.kernel_size.height} yield k + l * x.inputShape.width
        for {
          cIn <- 0 until x.inputShape.channel
          i <- t.neighbourExtractor.registers.indices.init if !windowIndices.contains(i)
          ri_o = t.neighbourExtractor.registers(i).output.id.replace(t.neighbourExtractor.id, s"${t.neighbourExtractor.id}_$cIn")
          rip1_i = t.neighbourExtractor.registers(i + 1).input.id.replace(t.neighbourExtractor.id, s"${t.neighbourExtractor.id}_$cIn")
        } {
            ri_o <=> rip1_i
        }
        result
      }


      def stuckAtClasses(x: Convolution2D): UnionFind[String] = {
        val t = x.transform(Stream.empty[Int])
        val result = UnionFind.empty[String]
        // Get failure scenarios for the convolution layer unit
        result ++= x.stuckAtScenarios
        // define input & output of convolution layer
        for {c <- 0 until x.inputShape.channel}
          result += s"${t.input.id}_$c"

        for {c <- 0 until x.outputShape.channel}
          result += s"${t.output.id}_$c"

        // apply rule 1 equivalence for dot product
        applyRule1EquivalenceDotProduct(result, t, x)
        // apply rule 1 equivalence for extractors
        applyRule1NExtr(result, x, t)

        result
      }


    }

    implicit val denseIsFaultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[Dense] = new FaultEquivalenceClassBuildable[Dense] {

      def stuckAtClasses(x: Dense): UnionFind[String] = {
        val t = x.transform(Stream.empty[Int])
        val result = UnionFind.empty[String]

        // get the set of failure scenarios
        result ++= x.stuckAtScenarios
        // add abstract scenarios for the inputs/outputs of the dense layer unit
        for {cIn <- 0 until x.inputShape.channel}
          result += s"${t.input.id}_$cIn"

        for {cOut <- 0 until x.outputShape.channel}
          result += s"${t.output.id}_$cOut"


        //ENCODING RULE 1
        encodeSaXEquivalenceRuleInDense(x, t, result)

        result
      }


      private def encodeSaXEquivalenceRuleInDense(x: Dense, t: Transformable.DenseChannelUnit[Int], result: UnionFind[String]): Unit = {
        import result._
        for {
          cOut <- 0 until x.outputShape.channel
          o = s"${t.output.id}_$cOut"
          outReg_o = t.outRegister.output.id.replace("outRegister", s"ncu_$cOut.outRegister")
          outReg_i = t.outRegister.input.id.replace("outRegister", s"ncu_$cOut.outRegister")
          biasAdd_o = s"${t.id}.ncu_$cOut.biasAdd.o"
          biasAdd_i = s"${t.id}.ncu_$cOut.biasAdd.i"
          accReg_resOut = t.accRegister.resultOut.id.replace("accRegister", s"ncu_$cOut.accRegister")
          accReg_i = t.accRegister.input.id.replace("accRegister", s"ncu_$cOut.accRegister")
          add_o = s"${t.id}.ncu_$cOut.add.o"
        } {
          o <=> outReg_o
          outReg_o <=> outReg_i
          outReg_i <=> biasAdd_o
          biasAdd_i <=> accReg_resOut
          add_o <=> accReg_i
          for {
            cIn <- 0 until x.inputShape.channel
            add_i = s"${t.id}.ncu_$cOut.add.i_$cIn"
            prodRegister_o = t.prodRegister.output.id.replace("prodRegister", s"ncu_$cOut.prodRegister_$cIn")
            prodRegister_i = t.prodRegister.input.id.replace("prodRegister", s"ncu_$cOut.prodRegister_$cIn")
            prod_o = s"${t.id}.ncu_$cOut.prod_$cIn.o"
          } {
            add_i <=> prodRegister_o
            prodRegister_o <=> prodRegister_i
            prod_o <=> prodRegister_i
          }
        }
      }

      def bitFlipClasses[U: Streamable](f: TFlow[U], x: Dense): UnionFind[String] = {
        val t = x.transform(f)

        val result = UnionFind.empty[String]
        result ++= x.bitFlipsScenarios(f)
        //An abstract BF is created for each I/O flow just to allow equivalence propagation among layers
        for {
          cIn <- 0 until x.inputShape.channel
          d <- t.input.tFlow.toDates
        }
          result += s"${t.input.id}_$cIn@$d"
        for {
          cOut <- 0 until x.outputShape.channel
          d <- t.output.tFlow.toDates
        }
          result += s"${t.output.id}_$cOut@$d"

        import result._

        // The only BF that are equivalent are output flows and extractorRegisters
        for {
          cOut <- 0 until x.outputShape.channel
          d <- t.output.tFlow.toDates
        } yield
          s"${t.output.id}_$cOut@$d" <=> s"${t.outRegister.id.replace(t.id, s"${t.id}.ncu_$cOut")}@$d"

        result
      }
    }

    implicit val sequencerIsFaultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[Sequencer] = new FaultEquivalenceClassBuildable[Sequencer] {

      def stuckAtClasses(x: Sequencer): UnionFind[String] = {
        val t = x.transform(Stream.empty[Int])
        val result = UnionFind.empty[String]
        result ++= x.stuckAtScenarios
        // Add abstract scenarios on inputs and outputs of the component
        for {cIn <- 0 until x.inputShape.channel}
          result += s"${t.input.id}_$cIn"

        for {cOut <- 0 until x.outputShape.channel}
          result += s"${t.output.id}_$cOut"

        import result._
        // Encode sax equivalence rule
        for {
          cOut <- 0 until x.outputShape.channel
          r <- t.registers
          r_i = r.input.id.replace(".r", s".piso_$cOut.r")
          r_o = r_i.replace(".i", ".o")
        } {
          r_i <=> r_o
        }
        for {
          cOut <- 0 until x.outputShape.channel
          r0_i = t.registers.head.input.id.replace(".r", s".piso_$cOut.r")
          rN_o = t.registers.last.input.id.replace(".r", s".piso_$cOut.r").replace(".i", ".o")
          output = s"${t.output.id}_$cOut"
          input = s"${t.input.id}_${cOut * (x.inputShape.channel / x.outputShape.channel)}"
        } {
          output <=> rN_o
          input <=> r0_i
        }
        //encode sax equivalence for multiplexers
        for {
          cOut <- 0 until x.outputShape.channel
          (r, id) <- t.registers.zipWithIndex.init
          ri_o = r.input.id.replace(".r", s".piso_$cOut.r").replace(".i", ".o")
          mi_i = r.input.id.replace(".r", s".piso_$cOut.mux")
          mi_o = mi_i.replace(".i", ".o")
          rip_i = r.output.id.replace(".r", s".piso_$cOut.r")
          input = s"${t.input.id}_${cOut * (x.inputShape.channel / x.outputShape.channel) + id + 1}"
        } {
          ri_o <=> mi_i + "1"
          rip_i <=> mi_o
          mi_i + "0" <=> input
        }
        result
      }

      private def getSaXScenarios(x: Sequencer, t: Transformable.SequencerLayerUnit[Int], result: UnionFind[String]): Unit = {
        for {
          cOut <- 0 until x.outputShape.channel
          r <- t.registers
          id <- List(r.input.id, r.output.id).map(_.replace(".r", s".batch_$cOut.r"))
        }
          result += id
      }

      def bitFlipClasses[U: Streamable](f: TFlow[U], x: Sequencer): UnionFind[String] = {
        val t = x.transform(f)
        val result = UnionFind.empty[String]

        // initialize union find with bitflips scenarios
        result ++= x.bitFlipsScenarios(f)
        //An abstract BF is created for each I/O flow just to allow equivalence propagation among layers
        for {
          cIn <- 0 until x.inputShape.channel
          d <- t.input.tFlow.toDates
        }
          result += s"${t.input.id}_$cIn@$d"

        for {
          cOut <- 0 until x.outputShape.channel
          d <- t.output.tFlow.toDates
        }
          result += s"${t.output.id}_$cOut@$d"

        import result._

        for {
          cOut <- 0 until x.outputShape.channel
          rIdx <- t.registers.indices
          (d_i, date_i) <- t.registers(rIdx).output.tFlow.indices.zip(t.registers(rIdx).output.tFlow.toDates)
          bf = s"${t.registers(rIdx).id.replace(".r", s".piso_$cOut.r")}@$date_i"
        } {
          //The injection @date_i on r can simulate an injection on r - 1 @date_i - 1
          if (rIdx >= 1 && t.registers(rIdx - 1).output.tFlow.size >= d_i && d_i - 1 >= 0) {
            val datePrevious = t.registers(rIdx - 1).output.tFlow.toDates(d_i - 1)
            bf <=> s"${t.registers(rIdx - 1).id.replace(".r", s".piso_$cOut.r")}@$datePrevious"
          }
        }

        //The injection on the last register is equivalent to the abstract injection on the output
        for {
          cOut <- 0 until x.outputShape.channel
          (d_i, date_i) <- t.registers.last.output.tFlow.indices.zip(t.registers.last.output.tFlow.toDates)
          bf = s"${t.registers.last.id.replace(".r", s".piso_$cOut.r")}@$date_i"
        }
          bf <=> s"${t.output.id}_$cOut@${t.output.tFlow.toDates(d_i)}"

        //The injection on the abstract injection on the input is equivalent to the injection on the fed register
        for {
          cIn <- 0 until x.inputShape.channel
          r = t.registers(cIn % t.registers.length)
          bf = s"${r.id.replace(".r", s".piso_${cIn / t.registers.length}.r")}@${r.output.tFlow.toDates.head}"
        }
          bf <=> s"${t.input.id}_$cIn@${t.input.tFlow.toDates.head}"

        result
      }
    }

    implicit val inputIsNotFaultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[Input2D] = new FaultEquivalenceClassBuildable[Input2D] {


      def stuckAtClasses(x: Input2D): UnionFind[String] = UnionFind.empty[String]

      def bitFlipClasses[U: Streamable](f: TFlow[U], x: Input2D): UnionFind[String] = UnionFind.empty[String]
    }

    implicit val layerIsFaultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[Layer2D] = new FaultEquivalenceClassBuildable[Layer2D] {


      def stuckAtClasses(x: Layer2D): UnionFind[String] = x match {
        case x: Convolution2D => x.stuckAtXClasses
        case x: Pooling2D => x.stuckAtXClasses
        case x: Dense => x.stuckAtXClasses
        case x: Input2D => x.stuckAtXClasses
        case x: Sequencer => x.stuckAtXClasses
      }


      def bitFlipClasses[U: Streamable](f: TFlow[U], x: Layer2D): UnionFind[String] = x match {
        case x: Convolution2D => x.bitFlipClasses(f)
        case x: Pooling2D => x.bitFlipClasses(f)
        case x: Dense => x.bitFlipClasses(f)
        case x: Input2D => x.bitFlipClasses(f)
        case x: Sequencer => x.bitFlipClasses(f)
      }
    }

    implicit def convolutionalNeuralNetworkIsFaultEquivalenceClassBuildable[T <: ConvolutionalNeuralNetwork]: FaultEquivalenceClassBuildable[T] = new FaultEquivalenceClassBuildable[T] {

      def stuckAtClasses(x: T): UnionFind[String] = {
        val result = x.layers.par.map(_.stuckAtXClasses).reduce(_ || _)
        import result._
        for {
          p <- x.layers.sliding(2)
          (l, r) = (p.head, p.last)
          c <- 0 until l.outputShape.channel
        } {
          s"${l.name}.o_$c" <=> s"${r.name}.i_$c"
        }
        result
      }


      def bitFlipClasses[U: Streamable](f: TFlow[U], x: T): UnionFind[String] = {
        val zero = x.input2D.transform(Stream.empty[Int])
        println(s"[INFO] Computing input flows of ${x.name}")
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

        val result = inputFlows.zip(x.layers).par.map(p => {
          println(s"[INFO] Start of ${p._2.name} internal equivalence classes computation")
          val classes = p._2.bitFlipClasses(p._1)
          println(s"[INFO] End of ${p._2.name} internal equivalence classes computation")
          classes
        }).reduce(_ || _)
        import result._
        println(s"[INFO] Propagation equivalence between layers")
        for {
          (List(l, r), rInput) <- x.layers.sliding(2).toList.zip(inputFlows.tail)
          c <- 0 until l.outputShape.channel
          d <- rInput.toDates
        } {
          s"${l.name}.o_$c@$d" <=> s"${r.name}.i_$c@$d"
        }
        result
      }
    }
  }

}
