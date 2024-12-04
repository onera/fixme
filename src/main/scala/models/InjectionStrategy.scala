package models

import operators.all._

sealed trait InjectionStrategy {

  def id: String

  val injectionTime: Double

  def injectionPoints(x: Layer2D): Array[String]

  def faultList(x: ConvolutionalNeuralNetwork): Set[String] = {
    (for {
      layer <- x.layers
    } yield {
      injectionPoints(layer).toSet
    }).reduce(_ ++ _)
  }

  override def toString: String = id
}

object InjectionStrategy {
  object Implicits {

    implicit object InputOnly extends InjectionStrategy {
      def id: String = "InputOnlyStrategy"

      val injectionTime: Double = 1.2E-3

      def injectionPoints(x: Layer2D): Array[String] = Array(s"${x.name}.i")

    }

    implicit object MyInjectionStrategy extends InjectionStrategy {
      def id: String = "MyInjectionStrategy"

      val injectionTime: Double = 1.2E-3

      def injectionPoints(x: Layer2D): Array[String] = x match {
        case l: Convolution2D =>
          val t = l.transform(Stream.empty[Int])
          Array(t.input.id, t.output.id)
        case l: Pooling2D =>
          val t = l.transform(Stream.empty[Int])
          Array(t.input.id, t.output.id)
        case l: Dense =>
          val t = l.transform(Stream.empty[Int])
          Array(t.input.id)
        case _: Input2D =>
          Array.empty
        case l: Sequencer =>
          val t = l.transform(Stream.empty[Int])
          Array(t.output.id)
      }
    }


    /**
     * Input, output based injection strategy
     */
    implicit object InputBased extends InjectionStrategy {

      def id: String = "InputBasedInjection"

      val injectionTime: Double = 1.2E-3

      def injectionPoints(x: Layer2D): Array[String] = x match {
        case l: Convolution2D =>
          val t = l.transform(Stream.empty[Int])
          if (l.inputShape.channel >= 2)
            Array(t.input.id, t.output.id)
          else
            Array(t.output.id)
        case l: Pooling2D =>
          val t = l.transform(Stream.empty[Int])
          Array(t.input.id, t.output.id)
        case l: Dense =>
          val t = l.transform(Stream.empty[Int])
          Array(t.input.id)
        case _: Input2D =>
          Array.empty
        case l: Sequencer =>
          val t = l.transform(Stream.empty[Int])
          Array(t.output.id)
      }
    }

    /**
     * Injection on input, outputs and on each first register in shift registers (so in all single registers)
     */
    implicit object FirstRegisterBased extends InjectionStrategy {

      val id: String = "FirstRegisterBasedInjection"

      val injectionTime: Double = 1.2E-3

      def injectionPoints(x: Layer2D): Array[String] = x match {
        case l: Convolution2D =>
          val t = l.transform(Stream.empty[Int])
          InputBased.injectionPoints(l) :+ t.convolutionChannelUnit.prodRegister.id :+ t.convolutionChannelUnit.addRegister.id
        case l: Pooling2D =>
          val t = l.transform(Stream.empty[Int])
          InputBased.injectionPoints(l) :+ t.horizontalExtractor.registers.head.id
        case l: Dense =>
          val t = l.transform(Stream.empty[Int])
          InputBased.injectionPoints(l) :+ t.prodRegister.id :+ t.accRegister.id
        case _: Input2D => Array.empty
        case l: Sequencer => InputBased.injectionPoints(l)
      }
    }

    /**
     * Injection only on registers providing an output to combinatorial components
     */
    implicit object OutputRegisterBased extends InjectionStrategy {

      val id: String = "OutputRegisterBasedInjection"

      val injectionTime: Double = 1.2E-3

      def injectionPoints(x: Layer2D): Array[String] = x match {
        case l: Convolution2D =>
          val t = l.transform(Stream.empty[Int])
          t.neighbourExtractor.shiftRegisters
            .map(_.size - 1)
            .map(i => t.neighbourExtractor.registers(i).id) :+
            t.convolutionChannelUnit.prodRegister.id :+
            t.convolutionChannelUnit.addRegister.id
        case l: Pooling2D =>
          val t = l.transform(Stream.empty[Int])
          t.verticalExtractor.shiftRegisters
            .map(_.size - 1)
            .map(i => t.verticalExtractor.registers(i).id) ++ t.horizontalExtractor.registers.map(_.id)
        case l: Dense =>
          val t = l.transform(Stream.empty[Int])
          Array(t.prodRegister.id, t.accRegister.id, t.outRegister.id)
        case _: Input2D => Array.empty
        case l: Sequencer =>
          val t = l.transform(Stream.empty[Int])
          Array(t.registers.last.id)
      }
    }
  }
}
