package exporters

import exporters.FileWriter.writeLogFile
import models.Campaign
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers.Parser
import parsers.Parser.{getCampaign, parseFunctionalClassesSet}
import utils.CampaignFiles
import utils.Dataset.MNIST
import utils.FileManager.getCampaignFiles

import java.io.File
import scala.io.Source

trait EquivalenceClassBuilder {
  val goldName = s"golden"

  protected def buildUnionFind[T](campaignFiles: CampaignFiles, by: Map[String, Double] => T): Set[Set[String]]

  protected def computeEquivalenceClassesForScore(data: Range, campaignPath: String, excluding: Seq[String]): Option[Set[Set[String]]] = {
    for {
      campaignFiles <- getCampaignFiles(new File(campaignPath), MNIST, filter = data, exclude = Some(excluding))
    } yield {
      buildUnionFind(campaignFiles, x => x)
    }
  }

  protected def computeEquivalenceClassesForOrder(data: Range, campaignPath: String, excluding: Seq[String]): Option[Set[Set[String]]] = {
    for {
      campaignFiles <- getCampaignFiles(new File(campaignPath), MNIST, filter = data, exclude = Some(excluding))
    } yield {
      buildUnionFind(campaignFiles, x => x.keys.toList.sortBy(x))
    }
  }

  protected def computeEquivalenceClassesForClassification(data: Range, campaignPath: String, excluding: Seq[String]): Option[Set[Set[String]]] = {
    for {
      campaignFiles <- getCampaignFiles(new File(campaignPath), MNIST, filter = data, exclude = Some(excluding))
    } yield {
      buildUnionFind(campaignFiles, x => x.maxBy(_._2)._1)
    }
  }

  protected def exportResults(name: String, classes: Set[Set[String]]): Option[File] = {
    val exportData = classes.map(c =>
      if (c.contains(goldName))
        goldName -> Map("size" -> (c.size - 1))
      else
        c.head -> Map("size" -> c.size)).toMap
    writeLogFile(name, exportData, List("classRepr", "size"))
  }

  protected def exportSummarized(name: String, classes: Set[Set[String]]): Option[File] = {
    writeLogFile(name, classes
      .groupBy(c => if (c.contains(goldName)) c.size - 1 else c.size)
      .filterKeys(_ >= 1)
      .transform((_, v) =>
        Map(
          "number of classes" -> v.size,
          "contains golden" -> (if (v.exists(_.contains(goldName))) 1 else 0))), List("class size", "number of classes", "contains golden"))
  }
}

trait UnionFindBasedBuilder extends EquivalenceClassBuilder {
  def buildUnionFind[T](campaignFiles: CampaignFiles, by: Map[String, Double] => T): Set[Set[String]] = {
    var nbUnionBuildOver = 0

    def updateBuild(): Unit = {
      synchronized {
        if (nbUnionBuildOver == 0) println("building UnionFind:")
        nbUnionBuildOver += 1
        if (nbUnionBuildOver % 100 == 0) println(s"${nbUnionBuildOver / 100}%")
      }
    }

    var nbUnionMerged = 0

    def updateMerged(): Unit = {
      synchronized {
        if (nbUnionMerged == 0) println("merging UnionFind:")
        nbUnionMerged += 1
        if (nbUnionMerged % 100 == 0) println(s"${nbUnionMerged / 100}%")
      }
    }

    (for {
      result <- campaignFiles.resultsFiles.toList.sortBy(_._1.getParentFile.getName).flatMap(p => p._2.map(m => (p._1, m))).par
    } yield {
      val source = Source.fromFile(result._2)
      val injections = Parser.parse(Seq(source), result._1, campaignFiles.modelFile, campaignFiles.labelDictionary).toStream
      val goldScores = injections.head.goldScores
      val grouped = (injections.map(c => by(c.scores) -> c.injectionPoint.toString) :+ (by(goldScores) -> goldName)).groupBy(_._1).mapValues(_.map(_._2))
      val unionFind = ForallDataNumericBasedEqualityExporter(grouped)
      source.close()
      updateBuild()
      unionFind
    }).reduce((l, r) => {
      updateMerged(); l && r
    }).equivalenceClasses
  }
}

trait StreamBasedBuilder extends EquivalenceClassBuilder {

  private def reduceStreams[T](l: Iterable[Stream[T]], r: Iterable[Stream[T]]): Iterable[Stream[T]] =
    for {
      lc <- l
      rc <- r
      nc = lc.intersect(rc) if nc.size >= 2
    } yield {
      nc
    }

  protected def buildUnionFind[T](campaignFiles: CampaignFiles, by: Map[String, Double] => T): Set[Set[String]] = {
    var nbUnionBuildOver = 0

    def updateBuild() = {
      synchronized {
        if (nbUnionBuildOver == 0) println("building UnionFind:")
        nbUnionBuildOver += 1
        if (nbUnionBuildOver % 100 == 0) println(s"${nbUnionBuildOver / 100}%")
      }
    }

    var nbUnionMerged = 0

    def updateMerged() = {
      synchronized {
        if (nbUnionMerged == 0) println("merging UnionFind:")
        nbUnionMerged += 1
        if (nbUnionMerged % 100 == 0) println(s"${nbUnionMerged / 100}%")
      }
    }

    val classes = (for {
      result <- campaignFiles.resultsFiles.toList.sortBy(_._1.getParentFile.getName).flatMap(p => p._2.map(m => (p._1, m))).par
    } yield {
      val source = Source.fromFile(result._2)
      val injections = Parser.parse(Seq(source), result._1, campaignFiles.modelFile, campaignFiles.labelDictionary).toStream
      val goldScores = injections.head.goldScores
      val grouped = (injections.map(c => by(c.scores) -> c.injectionPoint.toString) :+ (by(goldScores) -> goldName))
        .groupBy(_._1)
        .mapValues(_.map(_._2))
        .values
        .filter(_.size >= 2)
      source.close()
      updateBuild()
      grouped
    }).reduce((l, r) => {
      val res = reduceStreams(l, r); updateMerged(); res
    })
    classes
      .map(_.toSet)
      .toSet
  }
}

class ScoreEqualityClassesExporterKDTest extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-0" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForScore(range, "src/test/resources/leNet5", Seq("bitflips", "stuck_at_1"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesEqScoresSA0.csv", classes)
      exportSummarized("summarizedEqScoreSA0.csv", classes)
    }
  }

  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-1" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForScore(range, "src/test/resources/leNet5", Seq("bitflips", "stuck_at_0"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesEqScoresSA1.csv", classes)
      exportSummarized("summarizedEqScoreSA1.csv", classes)
    }
  }

  "FunctionalClasses" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForScore(range, "src/test/resources/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN
      exportResults("functionalClassesEqScoresBF.csv", classes)
      exportSummarized("summarizedEqScoreBF.csv", classes)
    }
  }
}

class ScoreOrderingClassesExporterKDTest extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-0" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForOrder(range, "src/test/resources/leNet5", Seq("bitflips", "stuck_at_1"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesOrderScoresSA0.csv", classes)
      exportSummarized("summarizedOrderScoreSA0.csv", classes)
    }
  }

  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-1" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForOrder(range, "src/test/resources/leNet5", Seq("bitflips", "stuck_at_0"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesOrderScoresSA1.csv", classes)
      exportSummarized("summarizedOrderScoreSA1.csv", classes)
    }
  }

  "FunctionalClasses" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForOrder(range, "src/test/resources/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesOrderScoresBF.csv", classes)
      exportSummarized("summarizedOrderScoreBF.csv", classes)
    }
  }
}

class MaxScoreClassesExporterKDTest extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-0" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForClassification(range, "src/test/resources/leNet5", Seq("bitflips", "stuck_at_1"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesClassificationSA0.csv", classes)
      exportSummarized("summarizedClassificationSA0.csv", classes)
    }
  }

  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-1" in {
    val range = 0 until 10
    for {classes <- computeEquivalenceClassesForClassification(range, "src/test/resources/leNet5", Seq("bitflips", "stuck_at_0"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesClassificationSA1.csv", classes)
      exportSummarized("summarizedClassificationSA1.csv", classes)
    }
  }

  "FunctionalClasses" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 1
    for {classes <- computeEquivalenceClassesForClassification(range, "src/test/resources/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesClassificationBF.csv", classes)
      exportSummarized("summarizedClassificationBF.csv", classes)
    }
  }
}

class ScoreEqualityClassesExporterKFGlobalTestBF extends AnyFlatSpec with should.Matchers with StreamBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForScore(range, "data/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesEqScoresBF.csv", classes)
      exportSummarized("summarizedEqScoreBF.csv", classes)
    }
  }
}

class ScoreEqualityClassesExporterKDGlobalTestSA0 extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-0" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForScore(range, "data/leNet5", Seq("bitflips", "stuck_at_1"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //FIXME IT SHOULD BE 1214 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesEqScoresSA0.csv", classes)
      exportSummarized("summarizedEqScoreSA0.csv", classes)
    }
  }
}

class ScoreEqualityClassesExporterKDGlobalTestSA1 extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-1" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForScore(range, "data/leNet5", Seq("bitflips", "stuck_at_0"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //FIXME IT SHOULD BE 1214 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesEqScoresSA1.csv", classes)
      exportSummarized("summarizedEqScoreSA1.csv", classes)
    }
  }
}

class ScoreOrderingClassesExporterKDGlobalTestBF extends AnyFlatSpec with should.Matchers with StreamBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForOrder(range, "data/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesOrderScoresBF.csv", classes)
      exportSummarized("summarizedOrderScoreBF.csv", classes)
    }
  }
}

class ScoreOrderingClassesExporterKDGlobalTestSA0 extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-0" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForOrder(range, "data/leNet5", Seq("bitflips", "stuck_at_1"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesOrderScoresSA0.csv", classes)
      exportSummarized("summarizedOrderScoreSA0.csv", classes)
    }
  }
}

class ScoreOrderingClassesExporterKDGlobalTestSA1 extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-1" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForOrder(range, "data/leNet5", Seq("bitflips", "stuck_at_0"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesOrderScoresSA1.csv", classes)
      exportSummarized("summarizedOrderScoreSA1.csv", classes)
    }
  }
}

class MaxScoreClassesExporterKDGlobalTestBF extends AnyFlatSpec with should.Matchers with StreamBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForClassification(range, "data/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesClassificationBF.csv", classes)
      exportSummarized("summarizedClassificationBF.csv", classes)
    }
  }
}

class MaxScoreClassesExporterKDGlobalTestSA0 extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-0" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForClassification(range, "data/leNet5", Seq("bitflips", "stuck_at_1"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesClassificationSA0.csv", classes)
      exportSummarized("summarizedClassificationSA0.csv", classes)
    }
  }
}

class MaxScoreClassesExporterKDGlobalTestSA1 extends AnyFlatSpec with should.Matchers with UnionFindBasedBuilder {
  "FunctionalClasses" should "compute functional classes in parallel and merge results for SA-1" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForClassification(range, "data/leNet5", Seq("bitflips", "stuck_at_0"))} {
      classes.toSeq.map(_.size).sum shouldBe 1217 //+1 for golden mark
      exportResults("functionalClassesClassificationSA1.csv", classes)
      exportSummarized("summarizedClassificationSA1.csv", classes)
    }
  }
}

class AllClassesExporterGlobalTestBF extends AnyFlatSpec with should.Matchers with StreamBasedBuilder {
  "Classification equivalence classes" should "be computed for BF on all data" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForClassification(range, "data/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesClassificationBF.csv", classes)
      exportSummarized("summarizedClassificationBF.csv", classes)
    }
  }

  "Order equivalence classes" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForOrder(range, "data/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesOrderScoresBF.csv", classes)
      exportSummarized("summarizedOrderScoreBF.csv", classes)
    }
  }

  "Score equivalence classes" should "compute functional classes in parallel and merge results for BF" in {
    val range = 0 until 10000
    for {classes <- computeEquivalenceClassesForScore(range, "data/leNet5", Seq("stuck_at_1", "stuck_at_0"))} {
      //FIXME  classes.toSeq.map(_.size).sum SHOULD BE 139 328 (+1 for golden mark) BUT FILE DOES CONTAIN 1200 exploitable lines
      exportResults("functionalClassesEqScoresBF.csv", classes)
      exportSummarized("summarizedEqScoreBF.csv", classes)
    }
  }
}

class ConsistencyWithIncrementalComputation extends AnyFlatSpec with should.Matchers with StreamBasedBuilder {
  "Score equivalence classes" should "be consistent with the ones computed with the incremental compute blocks" in {
    import operators.all._
    val range = 0 until 10000
    (for {
      campaignFiles <- getCampaignFiles(new File("data/leNet5"), MNIST, range, Some(Seq("stuck_at-1", "bitflip")))
      campaign <- getCampaign(campaignFiles)
      incrementallyComputedFile = campaign.computeScoreEquivalenceClasses("-fullSA0")
      incrementallyComputedClasses = parseFunctionalClassesSet(incrementallyComputedFile)
      classes <- computeEquivalenceClassesForScore(range, "data/leNet5", Seq("stuck_at_1", "bitflip"))
    } yield {
      classes shouldBe incrementallyComputedClasses
    }) shouldBe defined
  }
}

