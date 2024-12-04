package operators

import models._
import operators.Transformable.TFlow
import operators.all._
import utils.FileManager.locateTempFile

import scala.io.Source

trait Covered[T] {
  def coveredFMCount[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Int

  def coveredSaXCount(x: T, s: InjectionStrategy): Int

  def coveredBfCount[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Int

  def coverageRate[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Double

  def coverageSaXRate(x: T, s: InjectionStrategy): Double

  def coverageBfRate[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Double
  //    coveredFMCount(x, s, f) / (e.stuckAtCount(x) + e.bitFlipsCount(f, x))
}

object Covered {

  private def bitwidthPerNameInConv(s: String): Int =
    if (s.contains("dotProduct") && s.contains("prodRegister")) 24
    else if (s.contains("dotProduct") && s.contains("prod") && s.contains(".i")) 16
    else if (s.contains("dotProduct") && s.contains("prod") && s.contains(".o")) 24
    else if (s.contains("dotProduct") && s.contains("add.i")) 24
    else if (s.contains("dotProduct") && s.contains("add.o")) 32
    else if (s.contains("dotProduct") && s.contains("addRegister")) 32
    else if (s.contains("dotProduct") && s.contains("max")) 32
    else if (s.contains("extractor") && s.contains(".r")) 16
    else {
      //      println(s"[WARNING] $s does not belong to conv")
      0
    }

  private def bitwidthPerNameInDense(s: String): Int =
    if (s.contains("outRegister")) 16
    else if (s.contains("prodRegister")) 24
    else if (s.contains("prod") && s.contains(".o")) 24
    else if (s.contains("prod") && s.contains(".i")) 16
    else if (s.contains("add.i_acc")) 32
    else if (s.contains("add.i")) 24
    else if (s.contains("add.o")) 32
    else if (s.contains("accRegister")) 32
    else if (s.contains("biasAdd")) 32
    else if (s.contains("outRegister")) 16
    else {
      //      println(s"[WARNING] $s does not belong to dense")
      0
    }

  private def bitwidthPerNameInPooling(s: String): Int =
    if (s.contains("Extractor") && s.contains(".r")) 16
    else if (s.contains("Max")) 16
    else {
      //      println(s"[WARNING] $s does not belong to pooling")
      0
    }

  private def bitwidthPerNameInSequencer(s: String): Int =
    if (s.contains(".r") || s.contains("mux")) 16
    else {
      //      println(s"[WARNING] $s does not belong to sequencer")
      0
    }


  def bwConverter(layer: Layer2D, s: String) = layer match {
    case _: Convolution2D => bitwidthPerNameInConv(s)
    case _: Pooling2D => bitwidthPerNameInPooling(s)
    case _: Dense => bitwidthPerNameInDense(s)
    case _: Sequencer => bitwidthPerNameInSequencer(s)
    case _ => 0
  }

  def bwConverterNN(neuralNetwork: ConvolutionalNeuralNetwork, s: String) = {
    (for {
      layer <- neuralNetwork.layers.find(l => s.contains(l.name))
    } yield {
      layer match {
        case _: Convolution2D => bitwidthPerNameInConv(s)
        case _: Pooling2D => bitwidthPerNameInPooling(s)
        case _: Dense => bitwidthPerNameInDense(s)
        case _: Sequencer => bitwidthPerNameInSequencer(s)
        case _ => 0
      }
    }).getOrElse(0)
  }


  def getBfClasses[T: FaultEquivalenceClassBuildable, U: Streamable](x: T, f: TFlow[U]) = {
    val name = x match {
      case layer: Layer2D => layer.name
      case network: ConvolutionalNeuralNetwork => network.name
      case _ => "unknown"
    }
    ((for {
      bfFile <- locateTempFile(s"bf-${name}")
    } yield {
      //println("[INFO] Found previous BF equivalence classes checkpoint")
      val file = Source.fromFile(bfFile)
      val bfLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      bfLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val bfUnionFind = x.bitFlipClasses(f)
      ("bf-" + name).export(bfUnionFind)
      bfUnionFind.equivalenceClasses
    })
  }

  def getSAXClasses[T: FaultEquivalenceClassBuildable](x: T) = {
    val name = x match {
      case layer: Layer2D => layer.name
      case network: ConvolutionalNeuralNetwork => network.name
      case _ => "unknown"
    }
    ((for {
      saxFile <- locateTempFile(s"sax-${name}")
    } yield {
      //println("[INFO] Found previous SaX equivalence classes checkpoint")
      val file = Source.fromFile(saxFile)
      val saxLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      saxLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val saxUnionFind = x.stuckAtXClasses
      ("sax-" + name).export(saxUnionFind)
      saxUnionFind.equivalenceClasses
    })
  }
  trait Instances {

    implicit class CoveredOps[T](x: T)(implicit e: Covered[T]) {
      def coveredFMCount[U: Streamable](s: InjectionStrategy, f: TFlow[U]): Int = e.coveredFMCount(x, s, f)

      def coveredBfCount[U: Streamable](s: InjectionStrategy, f: TFlow[U]): Int = e.coveredBfCount(x, s, f)

      def coveredSaXCount(s: InjectionStrategy): Int = e.coveredSaXCount(x, s)

      def coverageRate[U: Streamable](s: InjectionStrategy, f: TFlow[U]): Double = e.coverageRate(x, s, f)

      def coverageSaXRate(s: InjectionStrategy): Double = e.coverageSaXRate(x, s)

      def coverageBfRate[U: Streamable](s: InjectionStrategy, f: TFlow[U]): Double = e.coverageBfRate(x, s, f)

    }

    implicit def faultClassBuildableIsCovered[T](implicit fallible: Fallible[T],
                                                 faultEquivalenceClassBuildable: FaultEquivalenceClassBuildable[T]) = new Covered[T] {


      def coveredFMCount[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Int = {
        val classes = getSAXClasses(x) ++ getBfClasses(x, f)
        computeCovered(x, s, classes)
      }

      private def computeCovered(x: T, s: InjectionStrategy, classes: Set[Set[String]]) = {
        x match {
          case layer2D: Layer2D => {
            val layerStrategy = s.injectionPoints(layer2D).toSet
            classes.foldLeft(0) { (acc, c) =>
              if (layerStrategy.toSeq.exists(injectionPoint => c.exists(e =>
                e.replaceAll("_[0-9]+", "")
                  .replaceAll("@[0-9]+", "")
                  .replaceAll(".ncu", "").contains(injectionPoint))))
                acc + c.toSeq.map(bwConverter(layer2D, _)).sum
              else
                acc
            }
          }
          case convolutionalNeuralNetwork: ConvolutionalNeuralNetwork => {
            val layerStrategy = s.faultList(convolutionalNeuralNetwork)
            classes.foldLeft(0) { (acc, c) =>
              if (layerStrategy.exists(injectionPoint => c.exists(e =>
                e.replaceAll("_[0-9]+", "")
                  .replaceAll("@[0-9]+", "")
                  .replaceAll(".ncu", "").contains(injectionPoint))))
                acc + c.toSeq.map(bwConverterNN(convolutionalNeuralNetwork, _)).sum
              else
                acc
            }
          }
          case _ => 0
        }
      }

      def coveredSaXCount(x: T, s: InjectionStrategy): Int = {
        val saClasses = getSAXClasses(x)
        computeCovered(x, s, saClasses)
      }

      def coveredBfCount[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Int = {
        val bfClasses = getBfClasses(x, f)
        computeCovered(x, s, bfClasses)
      }

      def coverageBfRate[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Double = coveredBfCount(x, s, f) * 100 / (x.bitFlipsCount(f)).toDouble
      def coverageSaXRate(x: T, s: InjectionStrategy): Double = coveredSaXCount(x, s) * 100 / (x.stuckAtCount).toDouble
      def coverageRate[U: Streamable](x: T, s: InjectionStrategy, f: TFlow[U]): Double = coveredFMCount(x, s, f) * 100 / (x.stuckAtCount + x.bitFlipsCount(f)).toDouble
    }

  }
}
