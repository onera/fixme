package operators

import models._
import operators.Transformable.TFlow
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers.CNNJsonParser
import utils.FileManager.{extractResourceAsFile, locateTempFile}

import java.io.File
import scala.io.Source

class CoveredTest extends AnyFlatSpec with should.Matchers {
  "Covered" should "return the coverage of an input only strategy on small convolution layer" in {
    val conv2d = Convolution2D("clu", Shape2D(3, 3), Shape2D(1, 1), Shape2D(2, 2), valid_padding = true, 1)
    val inputFlow = (1 to 9).toStream
    val strategy = InjectionStrategy.Implicits.InputOnly
    val saClasses = conv2d.stuckAtXClasses
    val bfClasses = conv2d.bitFlipClasses(inputFlow)
    val layerStrategy = conv2d match {
      case layer2D: Layer2D => strategy.injectionPoints(layer2D).toSet
      case _ => Set.empty[String]
    }
    val classesFiltered = (saClasses.equivalenceClasses ++ bfClasses.equivalenceClasses).filter(_.exists(_.contains("clu.i")))
    val coveredScenarios = classesFiltered.toSeq.map(_.size).sum
    val coveredFm = conv2d.coveredFMCount(strategy, inputFlow)
    val coverage = conv2d.coverageRate(strategy, inputFlow)

    println(coverage)
    coveredFm shouldBe coveredScenarios
  }

  "Coverage of the input register strategy" should "be consistent for the small convolution layer" in {
    val convolution2D = Convolution2D("clu", Shape2D(3, 3), Shape2D(1, 1), Shape2D(2, 2), valid_padding = true, 1)
    val inputFlow = (1 to 9).toStream
    val strategy = InjectionStrategy.Implicits.FirstRegisterBased
    val injectionPoints = strategy.injectionPoints(convolution2D)
    val saClasses = convolution2D.stuckAtXClasses
    val bfClasses = convolution2D.bitFlipClasses(inputFlow)
    val covered = convolution2D.coveredFMCount(strategy, inputFlow)
    val coverage = convolution2D.coverageRate(strategy, inputFlow)
    println(coverage)
  }
}

object ManuscriptFailuresTestSuite {
  val testCase0 = Convolution2D("clu", Shape2D(3, 3), Shape2D(1, 1), Shape2D(2, 2), valid_padding = true, 1)
  val testCase1 = Convolution2D("CONV1", Shape2D(32, 32), Shape2D(1, 1), Shape2D(5, 5), valid_padding = true, 6)
  val testCase2 = Pooling2D("POOL1", Shape2D(28, 28, 6), Shape2D(2, 2), Shape2D(2, 2), valid_padding = true)
  val testCase3 = Dense("FC1", Shape2D(5, 5, 16), Shape2D(1, 1, 120))
  val testCase4 = LeNetFixture.LeNetFixtureSafecompPaper.copy()

  val flowCase0 = Stream(1, 4, 6, 7, 8, 9, 11, 12, 14)
  val flowCase1 = (1 to 32 * 32).toStream
  val flowCase2 = (1 to 28 * 28).toStream
  val flowCase3 = (1 to 5 * 5).toStream
  val flowCase4 = flowCase1

  def getLayerSAXClasses(x: Layer2D) = {
    ((for {
      saxFile <- locateTempFile(s"sax-${x.name}")
    } yield {
      //println("[INFO] Found previous run checkpoint, bypassing equivalence classes computing")
      val file = Source.fromFile(saxFile)
      val saxLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      saxLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val saxUnionFind = x.stuckAtXClasses
      ("sax-" + x.name).export(saxUnionFind)
      saxUnionFind.equivalenceClasses
    })
  }

  def getLayerBfClasses(x: Layer2D, f: TFlow[Int]) = {
    ((for {
      bfFile <- locateTempFile(s"bf-${x.name}")
    } yield {
      //println("[INFO] Found previous run checkpoint, bypassing equivalence classes computing")
      val file = Source.fromFile(bfFile)
      val bfLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      bfLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val bfUnionFind = x.bitFlipClasses(f)
      ("bf-" + x.name).export(bfUnionFind)
      bfUnionFind.equivalenceClasses
    })
  }

  def coveredFmCount(l: Layer2D, classes: Set[Set[String]], injectionStrategy: Seq[String]): Int = {
    val bwConverter = l match {
      case _: Convolution2D => s: String => bitwidthPerNameInConv(s)
      case _: Pooling2D => s: String => bitwidthPerNameInPooling(s)
      case _: Dense => s: String => bitwidthPerNameInDense(s)
      case _: Sequencer => s: String => bitwidthPerNameInSequencer(s)
      case _ => s: String => 0
    }
    classes.foldLeft(0) { (acc, c) =>
      if (injectionStrategy.exists(injectionPoint => c.exists(e => e.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").matches(injectionPoint))))
        acc + c.toSeq.map(bwConverter).sum
      else
        acc
    }

  }

  def coverageRate(x: Layer2D, flow: TFlow[Int], injectionStrategy: InjectionStrategy): Double = {
    val classes = getLayerBfClasses(x, flow) ++ getLayerSAXClasses(x)
    val coveredFm = coveredFmCount(x, classes, injectionStrategy.injectionPoints(x).toSeq)
    val scenariosCount = x.stuckAtCount + x.bitFlipsCount(flow)
    coveredFm * 100.0 / scenariosCount
  }

  def getBfClasses(network: ConvolutionalNeuralNetwork, f: TFlow[Int]) = {
    {
      ((for {
        bfFile <- locateTempFile(s"bf-${network.name}")
      } yield {
        //println("[INFO] Found previous run checkpoint, bypassing equivalence classes computing")
        val file = Source.fromFile(bfFile)
        val bfLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
        file.close()
        bfLocalEq
      }).getOrElse {
        println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
        val bfUnionFind = network.bitFlipClasses(f)
        ("bf-" + network.name).export(bfUnionFind)
        bfUnionFind.equivalenceClasses
      })
    }
  }

  def getSAXClasses(network: ConvolutionalNeuralNetwork) = {
    ((for {
      saxFile <- locateTempFile(s"sax-${network.name}")
    } yield {
      //println("[INFO] Found previous run checkpoint, bypassing equivalence classes computing")
      val file = Source.fromFile(saxFile)
      val saxLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      saxLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val saxUnionFind = network.stuckAtXClasses
      ("sax-" + network.name).export(saxUnionFind)
      saxUnionFind.equivalenceClasses
    })
  }

  def bwConverter(layer: Layer2D): String => Int = {
    layer match {
      case _: Convolution2D => bitwidthPerNameInConv
      case _: Pooling2D => bitwidthPerNameInPooling
      case _: Dense => bitwidthPerNameInDense
      case _: Sequencer => bitwidthPerNameInSequencer
      case _ => s: String => 0
    }
  }

  def bwStringConverter(neuralNetwork: ConvolutionalNeuralNetwork) = (s: String) => {
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
  def coveredFmCountNN(neuralNetwork: ConvolutionalNeuralNetwork, classes: Set[Set[String]], injectionStrategy: Seq[String]) = {

    def bwConverter(s: String) = {
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

    classes.foldLeft(0) { (acc, c) =>
      if (injectionStrategy.exists(injectionPoint => c.exists(e => e.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").matches(injectionPoint))))
        acc + c.toSeq.map(bwConverter).sum
      else
        acc
    }
  }

  def coverageRateNN(x: ConvolutionalNeuralNetwork, flow: TFlow[Int], injectionStrategy: InjectionStrategy): Double = {
    val classes = getBfClasses(x, flow) ++ getSAXClasses(x)
    val injectionPoints = x.layers.foldLeft(Seq.empty[String]) { (acc, l) => acc ++ injectionStrategy.injectionPoints(l) }
    val coveredFm = coveredFmCountNN(x, classes, injectionPoints)
    val scenariosCount = x.stuckAtCount + x.bitFlipsCount(flow)
    coveredFm * 100.0 / scenariosCount
  }

  def bitwidthPerNameInConv(s: String): Int =
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

  def bitwidthPerNameInDense(s: String): Int =
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

  def bitwidthPerNameInPooling(s: String): Int =
    if (s.contains("Extractor") && s.contains(".r")) 16
    else if (s.contains("Max")) 16
    else {
      //      println(s"[WARNING] $s does not belong to pooling")
      0
    }

  def bitwidthPerNameInSequencer(s: String): Int =
    if (s.contains(".r") || s.contains("mux")) 16
    else {
      //      println(s"[WARNING] $s does not belong to sequencer")
      0
    }
}

class CoveredManuscriptTests extends AnyFlatSpec with should.Matchers {

  import ManuscriptFailuresTestSuite._

  val inputOnly = InjectionStrategy.Implicits.InputOnly
  val inputBased = InjectionStrategy.Implicits.InputBased
  val firstRegistersBased = InjectionStrategy.Implicits.FirstRegisterBased
  val outputRegistersBased = InjectionStrategy.Implicits.OutputRegisterBased

  def computeCoverages[T: Covered](x: T, f: TFlow[Int]): Unit = {
    println(s"For the component ${x}")

    println(s"\tCoverage (SA) input only: ${x.coverageSaXRate(inputOnly)}")
    println(s"\tCoverage (SA) first registers based: ${x.coverageSaXRate(firstRegistersBased)}")
    println(s"\tCoverage (SA) outputRegistersBased: ${x.coverageSaXRate(outputRegistersBased)}")

    println(s"\tCoverage (BF) input only: ${x.coverageBfRate(inputOnly, f)}")
    println(s"\tCoverage (BF) first registers based: ${x.coverageBfRate(firstRegistersBased, f)}")
    println(s"\tCoverage (BF) outputRegistersBased: ${x.coverageBfRate(outputRegistersBased, f)}")

  }


  "covered" should "work on a layer" in {
    val strat = outputRegistersBased
    val coveredBf = testCase0.coveredBfCount(strat, flowCase0)
    val totalBf = testCase0.bitFlipsCount(flowCase0)
    val coverage = testCase0.coverageBfRate(strat, flowCase0)
    println(s"covered: ${coverage}")
    coverage shouldBe coveredBf * 100.0 / totalBf

  }
  "the pre combinational register strategy" should "cover most of the failure modes in test case 0" in {
    val strat = outputRegistersBased
    val saxClasses = getLayerSAXClasses(testCase0)
    val injectedSaXClasses = saxClasses.toSeq.filter(c => c.exists(e => strat.injectionPoints(testCase0).exists(injectionPoint => e.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").contains(injectionPoint))))
    println(injectedSaXClasses.size)
    val bwConv = bwConverter(testCase0)
    val bitwidthsSaXClasses = injectedSaXClasses.map(_.map(e => (e, bwConv(e))))
    val count = bitwidthsSaXClasses.map(_.map(_._2).sum).sum
    println(count)
  }

  "the pre combinational register strategy" should "cover most of the failure modes in test case 3" in {
    val strat = outputRegistersBased
    val saxClasses = getLayerSAXClasses(testCase3)
    val injectedSaXClasses = saxClasses.toSeq.filter(c => c.exists(e => strat.injectionPoints(testCase3).exists(injectionPoint => e.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").contains(injectionPoint))))
    println(injectedSaXClasses.size)
    val bwConv = bwConverter(testCase3)
    val bitwidthsSaXClasses = injectedSaXClasses.map(_.map(e => (e, bwConv(e))))
    val count = bitwidthsSaXClasses.map(_.map(_._2).sum).sum
    println(count)
  }

  "the pre combinational register strategy" should "cover most of the bit-flips in test case 3" in {
    val strat = outputRegistersBased
    val bfClasses = getLayerBfClasses(testCase3, flowCase3)
    val injectedBfClasses = bfClasses.toSeq.filter(c => c.exists(e => strat.injectionPoints(testCase3).exists(injectionPoint => e.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").contains(injectionPoint))))
    println(injectedBfClasses.size)
    val bwConv = bwConverter(testCase3)
    val bitwidthsBfClasses = injectedBfClasses.map(_.map(e => (e, bwConv(e))))
    val count = bitwidthsBfClasses.map(_.map(_._2).sum).sum
    println(count)
  }

  "the pre combinational register strategy" should "cover most of the bit-flips in test case 2" in {
    val strat = outputRegistersBased
    val bfClasses = getLayerBfClasses(testCase2, flowCase2)
    val injectedBfClasses = bfClasses.toSeq.filter(c => c.exists(e => strat.injectionPoints(testCase2).exists(injectionPoint => e.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").contains(injectionPoint))))
    println(injectedBfClasses.size)
    val bwConv = bwConverter(testCase2)
    val bitwidthsBfClasses = injectedBfClasses.map(_.map(e => (e, bwConv(e))))
    val count = bitwidthsBfClasses.map(_.map(_._2).sum).sum
    println(count)
  }
  it should "compute coverage for the test suite and all injection strategies" in {
    computeCoverages(testCase0, flowCase0)
    computeCoverages(testCase1, flowCase1)
    computeCoverages(testCase2, flowCase2)
    computeCoverages(testCase3, flowCase3)
    computeCoverages(testCase4, flowCase4)
  }

  def computeInjectionStrategySize[T: FaultEquivalenceClassBuildable](x: T, injectionStrategy: InjectionStrategy, flow: TFlow[Int]) = {
    println(s"\t${injectionStrategy.id} strategy")
    val (saxClasses, bfClasses, strategy, bwFunction) = (x match {
      case l: Layer2D => {
        val bfClasses = getLayerBfClasses(l, flow)
        val saxClasses = getLayerSAXClasses(l)
        (saxClasses, bfClasses, injectionStrategy.injectionPoints(l).toSet, bwConverter(l))
      }
      case n: ConvolutionalNeuralNetwork => {
        val bfClasses = getBfClasses(n, flow)
        val saxClasses = getSAXClasses(n)
        (saxClasses, bfClasses, injectionStrategy.faultList(n), bwStringConverter(n))
      }
      case _ => {
        (Set.empty[Set[String]], Set.empty[Set[String]], Set.empty[String], (_: String) => 0)
      }
    })
    val mapClasses = Map("sax" -> saxClasses, "bf" -> bfClasses)
    val filteredClasses = mapClasses.mapValues(sc => sc.filter(c => c.exists(s => strategy.exists(p => s.replaceAll("_[0-9]+", "").replaceAll("@[0-9]+", "").replaceAll(".ncu", "").contains(p)))))
    val injectionPointsCount = filteredClasses.mapValues(_.toSeq.map(sc => sc.toSeq.map(bwFunction).max).sum)
    println(s"\t\tNumber of injections: ${injectionPointsCount}")
  }

  it should "compute the number of injections in each strategy for test case 0" in {
    println("Test case 0")
    computeInjectionStrategySize(testCase0, inputOnly, flowCase0)
    computeInjectionStrategySize(testCase0, firstRegistersBased, flowCase0)
    computeInjectionStrategySize(testCase0, outputRegistersBased, flowCase0)
  }

  it should "compute the number of injections in each strategy for test case 1" in {
    println("Test case 1")
    computeInjectionStrategySize(testCase1, inputOnly, flowCase1)
    computeInjectionStrategySize(testCase1, firstRegistersBased, flowCase1)
    computeInjectionStrategySize(testCase1, outputRegistersBased, flowCase1)
  }

  it should "compute the number of injections in each strategy for test case 2" in {
    println("Test case 2")
    computeInjectionStrategySize(testCase2, inputOnly, flowCase2)
    computeInjectionStrategySize(testCase2, firstRegistersBased, flowCase2)
    computeInjectionStrategySize(testCase2, outputRegistersBased, flowCase2)
  }

  it should "compute the number of injections in each strategy for test case 3" in {
    println("Test case 3")
    computeInjectionStrategySize(testCase3, inputOnly, flowCase3)
    computeInjectionStrategySize(testCase3, firstRegistersBased, flowCase3)
    computeInjectionStrategySize(testCase3, outputRegistersBased, flowCase3)
  }

  it should "compute the number of injections in each strategy for test case 4" in {
    println("Test case 4")
    computeInjectionStrategySize(testCase4, inputOnly, flowCase4)
    computeInjectionStrategySize(testCase4, firstRegistersBased, flowCase4)
    computeInjectionStrategySize(testCase4, outputRegistersBased, flowCase4)
  }

  it should "compute the number of failure mode of testCase0 covered by the input based strategy" in {
    val coveredBf = testCase0.coveredBfCount(inputOnly, flowCase0)
    val coveredSaX = testCase0.coveredSaXCount(inputBased)
    println(s"covered bf=${coveredBf}, covered sax=${coveredSaX}")
  }

}
