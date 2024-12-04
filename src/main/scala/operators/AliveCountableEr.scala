package operators

import models.Convolution2D
import operators.Transformable.{ConvolutionChannelUnit, ConvolutionLayerUnit, DenseChannelUnit, HierarchicalComponent, Extractor, PoolingLayerUnit, SequencerLayerUnit}

trait AliveCountableEr[T] {
def countAliveInstants(unit: T): Int
}

object AliveCountableEr {
  trait Instances {

    implicit class AliveCountableErOps[T](unit: T)(implicit aliveCountableEr: AliveCountableEr[T]) {
      def countEr: Int = aliveCountableEr.countAliveInstants(unit)
    }

    implicit def neighbourExtractorIsAliveCountableEr[T]: AliveCountableEr[Extractor[T]] = new AliveCountableEr[Extractor[T]] {

      def countAliveInstants(unit: Extractor[T]): Int = {
        (unit.registers.size-1 )* unit.input.tFlow.size + (unit.outputs.size -1) + unit.output.tFlow.size
      }
    }

    implicit def convolutionChannelUnitIsAliveCountableEr[T]: AliveCountableEr[ConvolutionChannelUnit[T]] =
      (unit: ConvolutionChannelUnit[T]) => unit.input.tFlow.size * unit.registers.size

    implicit def convolutionLayerUnitIsAliveCountableEr[T]: AliveCountableEr[ConvolutionLayerUnit[T]] =
      (unit: ConvolutionLayerUnit[T]) => unit.convolutionChannelUnit.countEr + unit.neighbourExtractor.countEr

    implicit def poolingLayerUnitIsAliveCountableEr[T]: AliveCountableEr[PoolingLayerUnit[T]] =
      (unit: PoolingLayerUnit[T]) => unit.verticalExtractor.countEr + unit.horizontalExtractor.countEr

    implicit def denseUnitIsAliveCountableEr[T]: AliveCountableEr[DenseChannelUnit[T]] =
      (unit: DenseChannelUnit[T]) => unit.input.tFlow.size + unit.output.tFlow.size

    implicit def sequencerLayerUnitIsAliveCountableEr[T]: AliveCountableEr[SequencerLayerUnit[T]] =
      (unit: SequencerLayerUnit[T]) => unit.output.tFlow.size*unit.registers.size
  }
}
