package operators

import models.{Convolution2D, Dense, Input2D, Layer2D, Pooling2D, Sequencer}
import operators.Fallible.vulnerableCount
import models.InjectionStrategy
import operators.Transformable.TFlow
import operators.all._
import scala.language.reflectiveCalls

trait Injected[T] {
  def injectedStuckAt(x: T)(implicit e: InjectionStrategy): Int

  def injectedBitFlips[I: Streamable](x: T, f: TFlow[I])(implicit e: InjectionStrategy): Int
}

object Injected {

  trait Instances {

    implicit class injectedOps[T](x: T) {

      def injectedStuckAt(implicit i: Injected[T], e: InjectionStrategy): Int = i.injectedStuckAt(x)

      def injectedBitFlips[I: Streamable](f: TFlow[I])(implicit i: Injected[T], e: InjectionStrategy): Int = i.injectedBitFlips(x, f)
    }

    implicit def convIsInjected(implicit s: InjectionStrategy): Injected[Convolution2D] = new Injected[Convolution2D] {

      def injectedStuckAt(x: Convolution2D)(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        //WARNING DOUBLE COUNT OF INPUT/OUTPUT INJECTION
        val t = x.transform(Stream.empty[Int])
        val injectedRegisters = t.neighbourExtractor.registers.map(_.id).count(points.contains) * 16
        val injectedInput = if (points.contains(t.input.id)) 16 else 0
        val injectedProd = if (points.contains(t.convolutionChannelUnit.prodRegister.id))
          x.inputShape.channel * x.kernel_size.height * x.kernel_size.width * 24
        else 0
        val injectedAdd = if (points.contains(t.convolutionChannelUnit.addRegister.id)) 32 else 0
        val injectedOutput = if (points.contains(t.output.id)) 16 else 0
        x.inputShape.channel * (injectedInput + injectedRegisters) +
          x.outputShape.channel * (injectedProd + injectedAdd + injectedOutput)
      }

      def injectedBitFlips[I: Streamable](x: Convolution2D, f: TFlow[I])(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val transformed = x.transform(f)
        val injectedInput = if (points.contains(transformed.input.id)) 16 * vulnerableCount(transformed.input.tFlow) else 0
        val injectedRegisters = 16 * transformed.neighbourExtractor.registers
          .filter(r => points.contains(r.id))
          .map(r => vulnerableCount(r.output.tFlow)).sum
        val injectedProd = if (points.contains(transformed.convolutionChannelUnit.prodRegister.id))
          x.inputShape.channel * x.kernel_size.height * x.kernel_size.width * 24 *
            vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
        else 0
        val injectedAdd = if (points.contains(transformed.convolutionChannelUnit.addRegister.id))
          32 * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow) else 0
        val injectedOutput = if (points.contains(transformed.output.id)) 16 * vulnerableCount(transformed.output.tFlow) else 0
        x.inputShape.channel * (injectedInput + injectedRegisters) +
          x.outputShape.channel * (injectedProd + injectedAdd + injectedOutput)
      }
    }

    implicit def poolIsInjected(implicit e: InjectionStrategy): Injected[Pooling2D] = new Injected[Pooling2D] {
      def injectedStuckAt(x: Pooling2D)(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val t = x.transform(Stream.empty[Int])
        val injectedRegisters = (t.verticalExtractor.registers ++ t.horizontalExtractor.registers).map(_.id).count(points.contains) * 16
        val injectedInput = if (points.contains(t.input.id)) 16 else 0
        val injectedOutput = if (points.contains(t.output.id)) 16 else 0
        x.inputShape.channel * (injectedRegisters + injectedInput + injectedOutput)
      }

      def injectedBitFlips[I: Streamable](x: Pooling2D, f: TFlow[I])(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val t = x.transform(f)
        val injectedRegisters = 16 * (t.verticalExtractor.registers ++ t.horizontalExtractor.registers)
          .filter(r => points.contains(r.id))
          .map(r => vulnerableCount(r.output.tFlow))
          .sum
        val injectedInput = if (points.contains(t.input.id)) 16 * vulnerableCount(t.inputTFlow) else 0
        val injectedOutput = if (points.contains(t.output.id)) 16 * vulnerableCount(t.output.tFlow) else 0
        x.inputShape.channel * (injectedRegisters + injectedInput + injectedOutput)
      }
    }

    implicit def denseIsInjected(implicit e: InjectionStrategy): Injected[Dense] = new Injected[Dense] {
      def injectedStuckAt(x: Dense)(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val t = x.transform(Stream.empty[Int])
        val inputInjected = if (points.contains(t.input.id)) x.inputShape.channel * 16 else 0
        val prodInjected = if (points.contains(t.prodRegister.id))
          x.inputShape.channel * 24
        else 0
        val addInjected = if (points.contains(t.accRegister.id)) 32 else 0
        val outRegisterInjected = if (points.contains(t.outRegister.id)) 32 else 0
        val outInjected = if (points.contains(t.output.id)) 16 else 0
        inputInjected + x.outputShape.channel * (prodInjected + addInjected + outInjected + outRegisterInjected)
      }

      def injectedBitFlips[I: Streamable](x: Dense, f: TFlow[I])(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val t = x.transform(f)
        val inputInjected = if (points.contains(t.input.id)) vulnerableCount(t.inputTFlow) * x.inputShape.channel * 16 else 0
        val prodInjected = if (points.contains(t.prodRegister.id))
          x.inputShape.channel * 24 * vulnerableCount(t.prodRegister.output.tFlow)
        else 0
        val addInjected = if (points.contains(t.accRegister.id))
          32 * vulnerableCount(t.accRegister.resultOut.tFlow) else 0
        val outRegisterInjected = if (points.contains(t.outRegister.id)) 32 * vulnerableCount(t.outRegister.output.tFlow) else 0
        val outInjected = if (points.contains(t.output.id)) 16 * vulnerableCount(t.output.tFlow) else 0
        inputInjected + x.outputShape.channel * (prodInjected + addInjected + outInjected + outRegisterInjected)
      }
    }

    implicit def sequencerIsInjected(implicit e: InjectionStrategy): Injected[Sequencer] = new Injected[Sequencer] {
      def injectedStuckAt(x: Sequencer)(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val t = x.transform(Stream.empty[Int])
        val inputInjected = if (points.contains(t.input.id)) 16 * t.registers.length else 0
        val outInjected = if (points.contains(t.output.id)) 16 else 0
        val registerInjected = t.registers.map(_.id).count(points.contains) * 16
        x.outputShape.channel * (inputInjected + outInjected + registerInjected)
      }

      def injectedBitFlips[I: Streamable](x: Sequencer, f: TFlow[I])(implicit e: InjectionStrategy): Int = {
        val points = e.injectionPoints(x)
        val t = x.transform(f)
        val inputInjected = if (points.contains(t.input.id)) 16 * t.registers.length * vulnerableCount(t.input.tFlow) else 0
        val outInjected = if (points.contains(t.output.id)) 16 * vulnerableCount(t.output.tFlow) else 0
        val registerInjected = t.registers
          .filter(r => points.contains(r.id))
          .map(r => vulnerableCount(r.output.tFlow)).sum * 16
        x.outputShape.channel * (inputInjected + outInjected + registerInjected)
      }
    }

    implicit def layerIsInjected(implicit e:InjectionStrategy): Injected[Layer2D] = new Injected[Layer2D] {
      def injectedStuckAt(x: Layer2D)(implicit e: InjectionStrategy): Int = x match {
        case y:Convolution2D => y.injectedStuckAt
        case y:Pooling2D => y.injectedStuckAt
        case y:Dense => y.injectedStuckAt
        case _:Input2D => 0
        case y:Sequencer => y.injectedStuckAt
      }
      def injectedBitFlips[I: Streamable](x: Layer2D, f: TFlow[I])(implicit e: InjectionStrategy): Int = x match {
        case y:Convolution2D => y.injectedBitFlips(f)
        case y:Pooling2D => y.injectedBitFlips(f)
        case y:Dense => y.injectedBitFlips(f)
        case _:Input2D => 0
        case y:Sequencer => y.injectedBitFlips(f)
      }
    }
  }
}
