package operators

import models._
import operators.Fallible.{stuckAtXRegister, vulnerableCount}
import operators.Transformable.{Extractor, TFlow}
import operators.all._

trait Injectable[T] {
  def injectableStuckAtX(x: T)(implicit strategy: InjectionStrategy): Int

  def injectableBitFlips[U: Streamable](f: TFlow[U], x: T)(implicit strategy: InjectionStrategy): Int
}

object Injectable {

  /**
   * This function filters the dates of f in shiftRegister or an extractor that can be captured by the corruption
   * of a set of other flows (corrupted by the considered injection point).
   *
   * @param f       the flow to analyse
   * @param injected    the flows that can be corrupted by an injection point
   * @param outputs the outputs that are corrupted by f
   * @return the dates where a corruption of f can be seen as a corruption of the next flows
   */
  private def injectable(f: TFlow[Element], injected: Array[TFlow[Element]], outputs: Array[TFlow[Element]]): TFlow[Element] =
    f.filter(p => {
      val corrupted = injected.flatMap(n => n.filter(_.dataId == p.dataId)).toSet
      val outputted = outputs.flatMap(n => n.filter(_.dataId == p.dataId)).toSet
      outputted.subsetOf(corrupted)
    })

  /**
   * This function filters the dates of f in shiftRegister or an extractor that result in a single corruption
   * on its output. A data d corrupted at a date t results in multiple corruptions on the output if the corrupted data
   * is in the register at various date of the output.
   *
   * @param f       the flow to analyse
   * @param next    the flow depending on f
   * @param outputs the outputs of the shift register
   * @return the date where a corruption of f results in a single output corruption
   */
  private def outputInjectable(f: TFlow[Element], next: Array[TFlow[Element]], outputs: Array[TFlow[Element]]): TFlow[Element] =
    f.filter(p => {
      val corrupted = next.flatMap(n => n.filter(_.dataId == p.dataId)).toSet
      val outputted = outputs.flatMap(n => n.filter(_.dataId == p.dataId)).toSet
      outputted.intersect(corrupted).size <= 1
    })

  private def applyCorruption(f: TFlow[CorruptibleElement], at: Int, next: Array[TFlow[CorruptibleElement]], outputs: Array[TFlow[CorruptibleElement]]): Array[TFlow[CorruptibleElement]] = {
    val CorruptibleElement(_, d, _) = f(at)
    val corrupted = next.flatMap(n => n.filter(_.dataId == d)).toSet
    outputs.map(o => o.map(e => if (corrupted.contains(e)) e.copy(corrupted = true) else e))
  }

  private def inject(on: Int, at: Int, e: Extractor[CorruptibleElement]): TFlow[Array[CorruptibleElement]] = {
    val f = e.registers(on).output.tFlow
    val indices = e.registers.indices.filter(k => k >= on && e.shiftRegisters.exists(_.size - 1 == k))
    val next = indices.map(k => e.registers(k).output.tFlow).toArray
    val result = applyCorruption(f, at, next, e.outputs.map(_.tFlow))
    result.head.indices.map(i => result.map(o => o(i))).toStream
  }

  def injectableExhaustive(e: Extractor[CorruptibleElement]): Array[TFlow[CorruptibleElement]] = {
    (for {
      i <- e.registers.indices
      f = e.registers(i).output.tFlow
    } yield {
      f.zipWithIndex.filter(p => {
        val (CorruptibleElement(_, d, _), k) = p
        !inject(i, k, e).exists(s => s.exists(p => !p.corrupted && p.dataId == d))
      }).map(_._1)
    }).toArray
  }

  def injectable(e: Extractor[Element]): Array[TFlow[Element]] = {
    val o = e.outputs.map(_.tFlow)
    (for {
      i <- e.registers.indices
      f = e.registers(i).output.tFlow
      indices = e.registers.indices.filter(k => k >= i && e.shiftRegisters.exists(_.size - 1 == k))
      next = indices.map(k => e.registers(k).output.tFlow).toArray
    } yield {
      injectable(f, next, o)
    }).toArray
  }

  def injectableOutput(e: Extractor[Element]): Array[TFlow[Element]] = {
    val o = e.outputs.map(_.tFlow)
    (for {
      i <- e.registers.indices
      f = e.registers(i).output.tFlow
      indices = e.registers.indices.filter(k => k >= i && e.shiftRegisters.exists(_.size - 1 == k))
      next = indices.map(k => e.registers(k).output.tFlow).toArray
    } yield {
      outputInjectable(f, next, o)
    }).toArray
  }

  /**
   * The function provide the dates on all the registers of e that can be emulated by a corruption
   * on the subset of extractor's register denoted injected.
   * @param e the extractor
   * @param injected the registers that can be corrupted
   * @return for each register of e, the corruptions that can be emulated
   */
  def injectableWithRegisters(e: Extractor[Element], injected:Array[Int]): Array[TFlow[Element]] = {
    for {
      i <- e.registers.indices.toArray
    }yield {
      if(injected.contains(i))
        e.registers(i).output.tFlow
      else {
        //not covered by upstream error injection
        val notUpStream =
        //keep the injection point before the studied one
          injected.filter(_ <= i)
            .foldLeft(e.registers(i).output.tFlow)((f,injectPoint) => {
              //the impacted outputs will be the one after the injection point
              val dismissedOutputs = e.shiftRegisters.count(_.size - 1 < injectPoint)
              val o = e.outputs.drop(dismissedOutputs).map(_.tFlow)
              //the covered dates will be the ones whose corruption always lead to a
              //corruption of impacted outputs
              val indices = e.registers.indices.filter(k => k >= i && e.shiftRegisters.exists(_.size - 1 == k))
              val next = indices.map(k => e.registers(k).output.tFlow).toArray
              val emulate = injectable(f, next, o)
              f.diff(emulate)
            })

        //not covered by a downstream error injection
        val notDownStream =  injected.filter(_ >= i)
          .foldLeft(e.registers(i).output.tFlow)((f,injectPoint) => {
            //the impacted output will be the ones after the studied register
            val dismissedOutputs = e.shiftRegisters.count(_.size - 1 < i)
            val o = e.outputs.drop(dismissedOutputs).map(_.tFlow)
            //the covered dates will be the ones such that the corruption of the registers after the injection point
            //covers all the corruptions of all the outputs after the studied register
            val indices = e.registers.indices.filter(k => k >= injectPoint && e.shiftRegisters.exists(_.size - 1 == k))
            val next = indices.map(k => e.registers(k).output.tFlow).toArray
            val emulate = injectable(f, next, o)
            f.diff(emulate)
          })
        e.registers(i).output.tFlow
          .filterNot(e => notUpStream.contains(e) && notDownStream.contains(e))
      }
    }
  }

  trait Instances {

    implicit class InjectableOps[T](x: T)(implicit ev: Injectable[T]) {
      def injectableStuckAtX(implicit strategy: InjectionStrategy): Int = ev.injectableStuckAtX(x)

      def injectableBitFlips[U: Streamable](f: TFlow[U])(implicit strategy: InjectionStrategy): Int = ev.injectableBitFlips(f, x)
    }

    implicit val convIsInjectable: Injectable[Convolution2D] = new Injectable[Convolution2D] {
      def injectableStuckAtX(x: Convolution2D)(implicit strategy: InjectionStrategy): Int = {
        val points = strategy.injectionPoints(x)
        val nbProd = x.inputShape.channel * x.kernel_size.height * x.kernel_size.width
        val t = x.transform(Stream.empty[Int])
        val maxOutInjectable = if(points.contains(t.output.id)) 32 else if(points.contains(t.convolutionChannelUnit.addRegister.id)) 31 else 0
        val maxInputInjectable = if(points.contains(t.convolutionChannelUnit.addRegister.id)) 32 else if(points.contains(t.output.id)) 31 else 0
        val reg32Injectable =
          if(points.contains(t.convolutionChannelUnit.addRegister.id)) stuckAtXRegister(32)
          else if(points.contains(t.output.id)) stuckAtXRegister(31) else 0
        val addOutputInjectable =
          if(points.contains(t.convolutionChannelUnit.addRegister.id)) 32
          else if (points.contains(t.output.id)) 31 else 0
        val addInputInjectable = if(points.contains(t.convolutionChannelUnit.prodRegister.id)) nbProd * 24 else 0
        val reg24Injectable = if(points.contains(t.convolutionChannelUnit.prodRegister.id)) stuckAtXRegister(24) else 0
        val prodOutputInjectable = if(points.contains(t.convolutionChannelUnit.prodRegister.id)) 24 else 0
        val prodInputInjectable = 0
        val firstRegExtractorInjectable = if(points.contains(t.neighbourExtractor.registers.head.id) | points.contains(t.input.id))
          stuckAtXRegister(16)
        else 0
        val extractorInjectable = t.neighbourExtractor.registers.tail.map(_.id).count(points.contains) * stuckAtXRegister(16)
        x.inputShape.channel * (extractorInjectable + firstRegExtractorInjectable)  +
          x.outputShape.channel * (
            maxOutInjectable +
              maxInputInjectable +
              reg32Injectable +
              addOutputInjectable +
              addInputInjectable +
              reg24Injectable * nbProd +
              prodOutputInjectable * nbProd +
              prodInputInjectable * nbProd)
      }

      def injectableBitFlips[U: Streamable](f: TFlow[U], x: Convolution2D)(implicit strategy: InjectionStrategy): Int = {
        val dates = f.toDates
        val points =  strategy.injectionPoints(x)
        val transformed = x.transform(dates.indices.map(i => Element(dates(i), i)).toStream)
        val firstInjectable = if(points.contains(transformed.input.id) || points.contains(transformed.neighbourExtractor.registers.head.id)) Array(0) else Array.empty
        val neighbourExtractorIndices = firstInjectable ++ points
          .filterNot(_ == transformed.neighbourExtractor.registers.head.id)
          .map(s => transformed.neighbourExtractor.registers.indexWhere(_.id == s))
          .filter(_ >= 0)
        val extractorInjectable = 16 * injectableWithRegisters(transformed.neighbourExtractor, neighbourExtractorIndices)
          .map(vulnerableCount)
          .sum
        val reg24Injectable = if(points.contains(transformed.convolutionChannelUnit.prodRegister.id))
          24 * x.inputShape.channel * x.kernel_size.height * x.kernel_size.width * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
        else 0
        val reg32Injectable = if(points.contains(transformed.convolutionChannelUnit.addRegister.id))
          32 * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
        else if (points.contains(transformed.output.id))
          31 * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
        else 0
        x.inputShape.channel * extractorInjectable  +  x.outputShape.channel * (reg24Injectable  + reg32Injectable )
      }
    }

    implicit val poolIsInjectable: Injectable[Pooling2D] = new Injectable[Pooling2D] {
      def injectableStuckAtX(x: Pooling2D)(implicit strategy: InjectionStrategy): Int = {
        val t = x.transform(Stream.empty[Int])
        val points = strategy.injectionPoints(x)
        val firstVerticalInjectable =
          if(points.contains(t.input.id) || points.contains(t.verticalExtractor.registers.head.id))
            stuckAtXRegister(16) else 0
        val registerInjectable = (t.horizontalExtractor.registers ++ t.verticalExtractor.registers.tail)
          .map(_.id)
          .count(points.contains)
        //vertical extractor max output stuckAt injectable by first register of horizontal extractor
        val verticalMaxOutputInjectable = if(points.contains(t.horizontalExtractor.registers.head.id)) 16 else 0
        val verticalMaxInputInjectable = 0
        val horizontalMaxInputInjectable = 0
        val horizontalMaxOutputInjectable = if(points.contains(t.output.id)) 16 else 0
        x.inputShape.channel *(
          registerInjectable * stuckAtXRegister(16) +
            verticalMaxOutputInjectable +
            verticalMaxInputInjectable +
            horizontalMaxInputInjectable +
            horizontalMaxOutputInjectable +
            firstVerticalInjectable
          )
      }

      def injectableBitFlips[U: Streamable](f: TFlow[U], x: Pooling2D)(implicit strategy: InjectionStrategy): Int = {
        val dates = f.toDates
        val injectionPoints = strategy.injectionPoints(x)
        val transformed = x.transform(dates.indices.map(i => Element(dates(i), i)).toStream)
        val verticalExtractorIndices = injectionPoints
          .map(s => if(s == transformed.input.id) 0 else transformed.verticalExtractor.registers.indexWhere(_.id == s))
          .filter(_ >= 0)
        val verticalInjectable = injectableWithRegisters(transformed.verticalExtractor,verticalExtractorIndices)
        val horizontalExtractorIndices = injectionPoints
          .map(s => transformed.horizontalExtractor.registers.indexWhere(_.id == s))
          .filter(_ >= 0)
        val horizontalInjectable = injectableWithRegisters(transformed.horizontalExtractor,horizontalExtractorIndices)
        x.inputShape.channel * (verticalInjectable ++ horizontalInjectable).map(vulnerableCount).sum * 16
      }
    }

    implicit val denseIsInjectable: Injectable[Dense] = new Injectable[Dense] {
      def injectableStuckAtX(x: Dense)(implicit strategy: InjectionStrategy): Int = {
        val points = strategy.injectionPoints(x)
        val t = x.transform(Stream.empty[Int])
        val prodInputInjectable = if(points.contains(t.input.id)) 0 else 0 //TODO Confirm Injection on all cout dot product => do not represent any local failure
        val prodOutputInjectable = if(points.contains(t.prodRegister.id)) 24 else 0
        val reg24Injectable = if(points.contains(t.prodRegister.id)) stuckAtXRegister(24) else 0
        val addProdInputInjectable = if(points.contains(t.prodRegister.id)) x.inputShape.channel * 24 else 0
        val addAccInputInjectable = if(points.contains(t.accRegister.id)) 32 else 0
        val accOutputInjectable = if(points.contains(t.accRegister.id)) 32 else 0
        val accInjectable = if(points.contains(t.accRegister.id)) stuckAtXRegister(32) else 0
        val addInputInjectable = if(points.contains(t.accRegister.id)) 32 else 0
        val addOutputInjectable = if(points.contains(t.accRegister.id) || points.contains(t.outRegister.id) || points.contains(t.output.id)) 32 else 0
        val outRegInjectable = if(points.contains(t.outRegister.id) || points.contains(t.output.id)) stuckAtXRegister(16) else 0
        x.outputShape.channel * (
          x.inputShape.channel * (prodInputInjectable + prodOutputInjectable + reg24Injectable) +
            addProdInputInjectable + addAccInputInjectable + accOutputInjectable + accInjectable +
            addInputInjectable + addOutputInjectable + outRegInjectable)
      }

      def injectableBitFlips[U: Streamable](f: TFlow[U], x: Dense)(implicit strategy: InjectionStrategy): Int = {
        val t = x.transform(f)
        val inputInjectable = 0
        val reg24Injectable = if(strategy.injectionPoints(x).contains(t.prodRegister.id))
          x.inputShape.channel * vulnerableCount(t.prodRegister.output.tFlow) * 24
        else 0
        val reg32Injectable = if(strategy.injectionPoints(x).contains(t.accRegister.id))
          32 * t.accRegister.outputs.map(o => vulnerableCount(o.tFlow)).sum
        else 0
        val reg16Injectable = if(strategy.injectionPoints(x).contains(t.outRegister.id) || strategy.injectionPoints(x).contains(t.output.id))
          16 * vulnerableCount(t.outRegister.output.tFlow)
        else 0
        x.outputShape.channel * (inputInjectable + reg24Injectable + reg32Injectable + reg16Injectable)
      }
    }

    implicit val sequencerIsInjectable: Injectable[Sequencer] = new Injectable[Sequencer] {
      def injectableStuckAtX(x: Sequencer)(implicit strategy: InjectionStrategy): Int = {
        //the stuckAt is injectable only on the first register (corruption of the first input <=> input injection)
        // or on the last register (corruption of all outputs <=> output injection)
        val t = x.transform(Stream.empty[Int])
        val firstReg = if(strategy.injectionPoints(x).contains(t.input.id) || strategy.injectionPoints(x).contains(t.registers.head.id))
          stuckAtXRegister(16) else 0
        val outReg = if(strategy.injectionPoints(x).contains(t.output.id) || strategy.injectionPoints(x).contains(t.registers.last.id))
          stuckAtXRegister(16) else 0
        val registerInjectable =
          t.registers.tail.init.map(_.id).count(strategy.injectionPoints(x).contains) * stuckAtXRegister(16)
        x.outputShape.channel * (firstReg + outReg + registerInjectable)
      }

      def injectableBitFlips[U: Streamable](f: TFlow[U], x: Sequencer)(implicit strategy: InjectionStrategy): Int = {
        val points = strategy.injectionPoints(x)
        val t = x.transform(f)
        if(points.contains(t.input.id) || points.contains(t.output.id) || points.contains(t.registers.last.id))
          x.bitFlipsCount(f)
        else if( t.registers.exists(r => points.contains(r.id))){
          val lastRegister = t.registers.filter(r => points.contains(r.id)).map(t.registers.indexOf).max
          //the n register can emulate the injection on the [0,n-1] previous ones
          16 * t.registers.take(lastRegister + 1).map(r => vulnerableCount(r.output.tFlow)).sum
        } else 0
      }
    }

    implicit def layerIsInjectable: Injectable[Layer2D] = new Injectable[Layer2D] {
      def injectableStuckAtX(x: Layer2D)(implicit strategy: InjectionStrategy): Int = x match {
        case x: Convolution2D => x.injectableStuckAtX
        case x: Pooling2D => x.injectableStuckAtX
        case x: Dense => x.injectableStuckAtX
        case _: Input2D => 0
        case x: Sequencer => x.injectableStuckAtX
      }

      def injectableBitFlips[U: Streamable](f: TFlow[U], x: Layer2D)(implicit strategy: InjectionStrategy): Int = x match {
        case x: Convolution2D => x.injectableBitFlips(f)
        case x: Pooling2D => x.injectableBitFlips(f)
        case x: Dense => x.injectableBitFlips(f)
        case _: Input2D => 0
        case x: Sequencer => x.injectableBitFlips(f)
      }
    }

    implicit def convolutionalNeuralNetworkIsInjectable[T <: ConvolutionalNeuralNetwork]: Injectable[T] = new Injectable[T] {

      def injectableStuckAtX(x: T)(implicit strategy: InjectionStrategy): Int = x.layers.map(_.injectableStuckAtX).sum

      def injectableBitFlips[U: Streamable](f: TFlow[U], x: T)(implicit strategy: InjectionStrategy): Int = {
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
        inputFlows.zip(x.layers).map(p => p._2.injectableBitFlips(p._1)).sum
      }
    }
  }
}
