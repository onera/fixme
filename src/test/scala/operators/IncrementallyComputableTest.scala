package operators

import exporters.FileWriter.writeLogClassesFile
import exporters.StreamBasedMeasure
import models._
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers.Parser.getCampaign
import utils.Dataset.MNIST
import utils.FaultType.{BITFLIP, FaultType, STUCK_AT_0, STUCK_AT_1}
import utils.FileManager.{clearLogs, clearTemps, getCampaignFiles}

import java.io.File
import scala.io.Source

trait IncrementallyComputableTest {
  protected def getCampaignTest(faultType: FaultType, fullTest: Boolean = false, clearTemp: Boolean = false) = {
    val range = if (fullTest) 0 until 10000 else 0 until 10
    val campaignPath = if (fullTest) "data/leNet5" else "src/test/resources/leNet5"
    val filters = faultType match {
      case STUCK_AT_0 => Seq("bitflips", "stuck_at_1")
      case STUCK_AT_1 => Seq("bitflips", "stuck_at_0")
      case BITFLIP => Seq("stuck_at_0", "stuck_at_1")
      case _ => Seq("bitflips", "stuck_at_0", "stuck_at_1")
    }
    if (clearTemp) {
      clearTemps()
      clearLogs()
    }
    for {
      campaignFiles <- getCampaignFiles(new File(campaignPath), MNIST, range, exclude = Some(filters))
      campaign <- getCampaign(campaignFiles)
    } yield {
      campaign
    }
  }
}

class ScoreEquivalenceClassesTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {
  val name = "test"
  val fullTest = false
  val clearLogs = true

  "IncrementalComputer" should "compute score equality equivalence classes for stuck-at-0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, fullTest, clearLogs)
    } yield {
      println("Computing score equality equivalence classes for stuck_at_0")
      campaign.computeScoreEquivalenceClasses("-${name}SA0")
    }
  }

  "IncrementalComputer" should "compute score equality equivalence classes for stuck-at-1" in {
    for {

      campaign <- getCampaignTest(STUCK_AT_1, fullTest, false)
    } yield {
      println("Computing score equality equivalence classes for stuck_at_1")
      campaign.computeScoreEquivalenceClasses(s"-${name}SA1")
    }
  }

  "IncrementalComputer" should "compute score equality equivalence classes for bitflips" in {
    for {
      campaign <- getCampaignTest(BITFLIP, fullTest, false)
    } yield {
      println("Computing score equality equivalence classes for bitflips")
      campaign.computeScoreEquivalenceClasses("-${name}BF")
    }
  }
}

class ScoreEquivalenceClassesFullTest extends ScoreEquivalenceClassesTest {
  override val name = "full"
  override val fullTest = true
  override val clearLogs = false
}


class IncrementallyComputableConsistencyTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest with StreamBasedMeasure {

  "IncrementalComputer" should "check consistency of ObservedMeasures for stuck-at-0 against test implementation" in {
    for {
      //      dir <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File("data/leNet5"), MNIST, exclude = Some(List("bitflip", "stuck_at_1")))
      campaign <- getCampaign(campaignFiles)
    } yield {
      //  clearLogs()
      val file = campaign.computeGlobalMeasures()

      val source = Source.fromFile(file)
      val observedIncremental = source.parse[Stream[GlobalMeasures]]
      for {
        incrementalFile <- "SA0".export(ObservedMetrics(observedIncremental))
        oldFile <- computeMeasure(toObservedMeasures, "ETSObservedMeasuresSA0.csv", STUCK_AT_0, fullTest = true)
      } {
        val s1 = Source.fromFile(oldFile)
        val s2 = Source.fromFile(incrementalFile)
        val contentOld = s1.getLines().map(_.split(',')).toList
        val contentNew = s2.getLines().map(_.split(',')).toList
        s1.close()
        s2.close()
        contentNew shouldBe contentOld
      }
    }
  }
}

class ScoreEqualityMapBasedTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {

  "IncrementalComputer" should "compute score equality equivalence classes for all stuck-at-0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, true, false)
    } yield {
      println("Computing score equality equivalence classes for stuck_at_0")
      campaign.computeScoreEquivalenceClassesMap("-fullSA0")
    }
  }
  "IncrementalComputer" should "compute score equality equivalence classes for stuck-at-1" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_1, true, false)
    } yield {
      println("Computing score equality equivalence classes for stuck_at_1")
      campaign.computeScoreEquivalenceClassesMap("-fullSA1")
    }
  }

  "IncrementalComputer" should "compute score equality equivalence classes for bitflips" in {
    for {
      campaign <- getCampaignTest(BITFLIP, true, false)
    } yield {
      println("Computing score equality equivalence classes for bitflips")
      campaign.computeScoreEquivalenceClassesMap("-fullBF")
    }
  }
}


class MeasuresIncrementallyComputedTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {

  "IncrementalComputer" should "compute data measures for test stuck-at-0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, false, true)
    } yield {
      println("Computing data measures for stuck_at_0")
      campaign.computeDataMeasures("-testSA0")
    }
  }


  "IncrementalComputer" should "compute data measures for test stuck-at-1" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_1, false, true)
    } yield {
      println("Computing data measures for stuck_at_1")
      campaign.computeDataMeasures("-testSA1")
    }
  }

  "IncrementalComputer" should "compute data measures for test bitflips" in {
    for {
      campaign <- getCampaignTest(BITFLIP, false, true)
    } yield {
      println("Computing data measures for bitflips")
      campaign.computeDataMeasures("-testBF")
    }
  }

}

class InjectionMeasuresIncrementallyComputedTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {

  "IncrementalComputer" should "compute injection measures for test stuck-at-0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, false, true)
    } yield {
      println("Computing injection measures for stuck_at_0")
      campaign.computeInjectionMeasures("-testSA0")
    }
  }


  "IncrementalComputer" should "compute injection measures for test stuck-at-1" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_1, false, true)
    } yield {
      println("Computing injection measures for stuck_at_1")
      campaign.computeInjectionMeasures("-testSA1")
    }
  }

  "IncrementalComputer" should "compute injection measures for test bitflips" in {
    for {
      campaign <- getCampaignTest(BITFLIP, false, true)
    } yield {
      println("Computing injection measures for bitflips")
      campaign.computeInjectionMeasures("-testBF")
    }
  }

}


class MeasuresIncrementallyComputedFullTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {

  "IncrementalComputer" should "compute data measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, true, false)
    } yield {
      println("Computing data measures for stuck_at_0")
      campaign.computeDataMeasures("-fullSA0")
    }
  }


  "IncrementalComputer" should "compute data measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_1, true, false)
    } yield {
      println("Computing data measures for stuck_at_1")
      campaign.computeDataMeasures("-fullSA1")
    }
  }

  "IncrementalComputer" should "compute data measures for full bitflips" in {
    for {
      campaign <- getCampaignTest(BITFLIP, true, false)
    } yield {
      println("Computing data measures for bitflips")
      campaign.computeDataMeasures("-fullBF")
    }
  }

}

class InjectionMeasuresIncrementallyComputedFullTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {

  "IncrementalComputer" should "compute injection measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, true, false)
    } yield {
      println("Computing injection measures for stuck_at_0")
      campaign.computeInjectionMeasures("-fullSA0")
    }
  }


  "IncrementalComputer" should "compute injection measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_1, true, false)
    } yield {
      println("Computing injection measures for stuck_at_1")
      campaign.computeInjectionMeasures("-fullSA1")
    }
  }

  "IncrementalComputer" should "compute injection measures for full bitflips" in {
    for {
      campaign <- getCampaignTest(BITFLIP, true, false)
    } yield {
      println("Computing injection measures for bitflips")
      campaign.computeInjectionMeasures("-fullBF")
    }
  }

}

//class ScoreEquivalentPostProcess extends AnyFlatSpec with should.Matchers {
//  "Score equivalent classes" should "be reformatted" in {
//    val s = Source.fromFile("experimentalResults/scoreEquivalenceClassesMap-fullBF.csv")
//    val classesEqByPoint = s.parse[ScoreEqualityEquivalenceClassesMap]
//    val classes = classesEqByPoint.classes.groupBy(_._2).keys.toSeq
//    writeLogClassesFile("scoreEquivalenceClassesMap-fullBF_reformatted", classes)
//    val count = classes.groupBy(_.size).mapValues(_.size)
//    println(count)
//    print(count.map(p => p._2 * p._1).sum)
//    s.close()
//  }
//}

trait MeasuresOnScoreEqualityClasses extends IncrementallyComputableTest {
  def getCampaignFiltered(faultType: FaultType, fullTest: Boolean = true, clearLogs: Boolean = false) = for {
    campaign <- getCampaignTest(faultType, fullTest, clearLogs)

    classesFile = campaign.computeScoreEquivalenceClassesMap(faultType match {
      case STUCK_AT_1 => "-fullSA1"
      case STUCK_AT_0 => "-fullSA0"
      case BITFLIP => "-fullBF"
    })
  } yield {
    val s = Source.fromFile(classesFile)
    val classesEqByPoint = s.parse[ScoreEqualityEquivalenceClassesMap]
    val toRemove = classesEqByPoint.classes.groupBy(_._2).keys.toSeq.flatMap(_.drop(1))
    val filteredCampaign = campaign.copy(strategy = campaign.strategy.mapValues(_.filterNot(i => toRemove.contains(i._2.toString))))
    s.close()
    filteredCampaign
  }
}

class MeasuresOnScoreEqualityClassesTest extends AnyFlatSpec with MeasuresOnScoreEqualityClasses with should.Matchers {
  val fullTestEnable = false
  val clearLogs = true
  val name = "test"

  "IncrementalComputer" should "compute observed measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, fullTestEnable, clearLogs)
    } yield {
      println("Computing observed measures for stuck_at_0")
      val file = campaign.computeGlobalMeasures(s"-classes${name}SA0")
      val s = Source.fromFile(file)
      println(s"-${name}SA0".export(ObservedMetrics(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }


  it should "compute observed measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_1, fullTestEnable, false)
    } yield {
      println("Computing observed measures for stuck_at_1")
      val file = campaign.computeGlobalMeasures(s"-classes${name}SA1")
      val s = Source.fromFile(file)
      println(s"-${name}SA1".export(ObservedMetrics(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }

  it should "compute observed measures for full bitflips" in {
    for {
      campaign <- getCampaignFiltered(BITFLIP, fullTestEnable, false)
    } yield {
      println("Computing observed measures for bit-flips")
      val file = campaign.computeGlobalMeasures(s"-classes${name}BF")
      val s = Source.fromFile(file)
      println(s"-${name}BF".export(ObservedMetrics(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }

  it should "compute misClassification measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, fullTestEnable, clearLogs)
    } yield {
      println("Computing misClassification measures for stuck_at_0")
      val file = campaign.computeGlobalMeasures(s"-classes${name}SA0")
      val s = Source.fromFile(file)
      println(s"-${name}SA0".export(MisClassificationMetrics(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }


  it should "compute misClassification measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_1, fullTestEnable, false)
    } yield {
      println("Computing misClassification measures for stuck_at_1")
      val file = campaign.computeGlobalMeasures(s"-classes${name}SA1")
      val s = Source.fromFile(file)
      println(s"-${name}SA1".export(MisClassificationMetrics(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }

  it should "compute misClassification measures for full bitflips" in {
    for {
      campaign <- getCampaignFiltered(BITFLIP, fullTestEnable, false)
    } yield {
      println("Computing misClassification measures for bit-flips")
      val file = campaign.computeGlobalMeasures(s"-classes${name}BF")
      val s = Source.fromFile(file)
      println(s"-${name}BF".export(MisClassificationMetrics(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }

  it should "compute total impact count measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, fullTestEnable, clearLogs)
    } yield {
      println("Computing total impact count measures for stuck_at_0")
      val file = campaign.computeGlobalMeasures(s"-classes${name}SA0")
      val s = Source.fromFile(file)
      println(s"-${name}SA0".export(ImpactCountTotalMeasures(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }


  it should "compute total impact count measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_1, fullTestEnable, false)
    } yield {
      println("Computing total impact count measures for stuck_at_1")
      val file = campaign.computeGlobalMeasures(s"-classes${name}SA1")
      val s = Source.fromFile(file)
      println(s"-${name}SA1".export(ImpactCountTotalMeasures(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }

  it should "compute total impact count measures for full bit flips" in {
    for {
      campaign <- getCampaignFiltered(BITFLIP, fullTestEnable, false)
    } yield {
      println("Computing total impact count measures for bit-flips")
      val file = campaign.computeGlobalMeasures(s"-classes${name}BF")
      val s = Source.fromFile(file)
      println(s"-${name}BF".export(ImpactCountTotalMeasures(s.parse[Stream[GlobalMeasures]])))
      s.close()
    }
  }

  it should "compute injection measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, fullTestEnable, false)
    } yield {
      println("Computing injection measures for stuck_at_0")
      campaign.computeInjectionMeasures(s"-classes${name}SA0")
    }
  }


  it should "compute injection measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_1, fullTestEnable, false)
    } yield {
      println("Computing injection measures for stuck_at_1")
      campaign.computeInjectionMeasures(s"-classes${name}SA1")
    }
  }

  it should "compute injection measures for full bitflips" in {
    for {
      campaign <- getCampaignFiltered(BITFLIP, fullTestEnable, false)
    } yield {
      println("Computing injection measures for bitflips")
      campaign.computeInjectionMeasures(s"-classes${name}BF")
    }
  }

  it should "compute data measures for full stuck-at-0" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, fullTestEnable, clearLogs = false)
    } yield {
      println("Computing data measures for stuck_at_0")
      campaign.computeDataMeasures(s"-classes${name}SA0")
    }
  }


  it should "compute data measures for full stuck-at-1" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_1, fullTestEnable, clearLogs = false)
    } yield {
      println("Computing data measures for stuck_at_1")
      campaign.computeDataMeasures(s"-classes${name}SA1")
    }
  }

  it should "compute data measures for full bitflips" in {
    for {
      campaign <- getCampaignFiltered(BITFLIP, fullTestEnable, clearLogs = false)
    } yield {
      println("Computing data measures for bitflips")
      campaign.computeDataMeasures(s"-classes${name}BF")
    }
  }
}

class MeasuresOnScoreEqualityClassesFullTest extends MeasuresOnScoreEqualityClassesTest {
  override val fullTestEnable: Boolean = true
  override val clearLogs: Boolean = false
  override val name = "full"
}

class ResetTests extends AnyFlatSpec with MeasuresOnScoreEqualityClasses {
  it should "reset temp and log dir" in {
    clearTemps()
    clearLogs()
  }

  it should "reset data measures only" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, fullTest = false, clearLogs = true)
      campaignInitial <- getCampaignTest(STUCK_AT_0, false)
    } {
      campaign.computeDataMeasures(s"-classesTestSA0")
      campaign.reset[Stream[DataMeasures]]("dataMeasures-classesTestSA0")
      campaignInitial.computeScoreEquivalenceClassesMap(s"-fullSA0")
      campaign.computeDataMeasures(s"-classesTestSA0")
    }
  }
}

class ScoreEquivalenceClassesSizes extends AnyFlatSpec with MeasuresOnScoreEqualityClasses {
  "scoreEquality size post process" should "compute the size of classes for stuck-at 0" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_0, true)
      resultClassesFile = campaign.computeScoreEquivalenceClassesMap("-fullSA0")
    } {
      val s = Source.fromFile(resultClassesFile)
      val classesEqByPoint = s.parse[ScoreEqualityEquivalenceClassesMap]
      val classes = classesEqByPoint.classes.groupBy(_._2).keys.toSeq
      val count = classes.groupBy(_.size).mapValues(_.size)
      println(count)
      print(count.map(p => p._2 * p._1).sum)
      s.close()
    }
  }
  it should "compute the size of classes for stuck-at 1" in {
    for {
      campaign <- getCampaignFiltered(STUCK_AT_1, fullTest = true)
      resultClassesFile = campaign.computeScoreEquivalenceClassesMap("-fullSA1")
    } {
      val s = Source.fromFile(resultClassesFile)
      val classesEqByPoint = s.parse[ScoreEqualityEquivalenceClassesMap]
      val classes = classesEqByPoint.classes.groupBy(_._2).keys.toSeq
      val count = classes.groupBy(_.size).mapValues(_.size)
      println(count)
      print(count.map(p => p._2 * p._1).sum)
      s.close()
    }
  }

  it should "compute the size of classes for bit-flips" in {
    for {
      campaign <- getCampaignFiltered(BITFLIP, fullTest = true)
      resultClassesFile = campaign.computeScoreEquivalenceClassesMap("-fullBF")
    } {
      val s = Source.fromFile(resultClassesFile)
      val classesEqByPoint = s.parse[ScoreEqualityEquivalenceClassesMap]
      val classes = classesEqByPoint.classes.groupBy(_._2).keys.toSeq
      val count = classes.groupBy(_.size).mapValues(_.size)
      println(count)
      print(count.map(p => p._2 * p._1).sum)
      s.close()
    }
  }
}

class AccuracyMeasuresTest extends AnyFlatSpec with should.Matchers with IncrementallyComputableTest {
  "AccuracyMeasures" should "be computed on stuck_at_0" in {
    for {
      campaign <- getCampaignTest(STUCK_AT_0, true, true)
    }
      campaign.computeAccuracyMeasures("-fullSA0")
  }
}