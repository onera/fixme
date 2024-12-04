package exporters

import exporters.FileWriter.writeLogFile
import models.CampaignResult
import indicators.CampaignResultOperators._
import parsers.Parser
import utils.CampaignFiles
import utils.Dataset.MNIST
import utils.FaultType.FaultType
import utils.FileManager.getCampaignFiles

import java.io.File
import scala.io.Source

trait StreamBasedMeasure {

  trait Measures {
    def combine(that: Measures): Measures

    def toMap: Map[String, Int]
  }

  case class MisClassificationMeasures(
                                        atLeastOneMisClassification: Boolean,
                                        maxRankDowngrade: Int) extends Measures {
    def combine(that: Measures): MisClassificationMeasures = that match {
      case t: MisClassificationMeasures =>
        MisClassificationMeasures(
          atLeastOneMisClassification || t.atLeastOneMisClassification,
          maxRankDowngrade max t.maxRankDowngrade)
      case _ => this
    }

    def toMap: Map[String, Int] =
      (0 to 9).map(i => s"$i-misc" -> (if (maxRankDowngrade == i && atLeastOneMisClassification) 1 else 0)).toMap +
        ("MisClassification" -> (if (atLeastOneMisClassification) 1 else 0))
  }

  object MisClassificationMeasures {
    val measureNames: List[String] = "MisClassification" +: (0 to 9).map(i => s"$i-misc").toList

    def apply(x: CampaignResult): MisClassificationMeasures = {
      val observedClassification = x.scores.keys.maxBy(x.scores)
      val expectedClassification = x.goldScores.keys.maxBy(x.goldScores)
      MisClassificationMeasures(
        observedClassification != expectedClassification,
        x.scores.size - 1 - x.scores.keys.toSeq.sortBy(x.scores).indexOf(expectedClassification)
      )
    }
  }

  case class ObservedMeasuresOld(
                                  atLeastOneScoreModif: Boolean,
                                  alwaysPreserveClassif: Boolean,
                                  alwaysTopScoreDegradedLeq: List[Boolean],
                                  alwaysScoreModifiedLeq: List[Boolean],
                                  alwaysRankModifiedLeq: List[Boolean],
                                  alwaysSoftMaxDegradedLeq: List[Boolean]
                                ) extends Measures {

    def combine(that: Measures): ObservedMeasuresOld = that match {
      case t: ObservedMeasuresOld => ObservedMeasuresOld(
        atLeastOneScoreModif || t.atLeastOneScoreModif,
        alwaysPreserveClassif && t.alwaysPreserveClassif,
        alwaysTopScoreDegradedLeq.zip(t.alwaysTopScoreDegradedLeq).map(p => p._1 && p._2),
        alwaysScoreModifiedLeq.zip(t.alwaysScoreModifiedLeq).map(p => p._1 && p._2),
        alwaysRankModifiedLeq.zip(t.alwaysRankModifiedLeq).map(p => p._1 && p._2),
        alwaysSoftMaxDegradedLeq.zip(t.alwaysSoftMaxDegradedLeq).map(p => p._1 && p._2)
      )
      case _ => this
    }

    private def toInt(b: Boolean): Int = if (b) 1 else 0

    def toMap: Map[String, Int] = {
      Map("Observed" -> toInt(atLeastOneScoreModif && alwaysPreserveClassif)) ++
        scoreDegradationList
          .zipWithIndex
          .map(i =>
            s"TopScoreDegraded<=${i._1}" ->
              toInt(atLeastOneScoreModif && alwaysPreserveClassif && alwaysTopScoreDegradedLeq(i._2))
          ).toMap ++
        scoreModifiedList
          .zipWithIndex
          .map(i =>
            s"scoreModified<=${i._1}" ->
              toInt(atLeastOneScoreModif && alwaysPreserveClassif && alwaysScoreModifiedLeq(i._2))
          ).toMap ++
        rankModifiedList
          .zipWithIndex
          .map(i =>
            s"rankModified<=${i._1}" ->
              toInt(atLeastOneScoreModif && alwaysPreserveClassif && alwaysRankModifiedLeq(i._2))
          ).toMap ++
        softmaxDegradationList
          .zipWithIndex
          .map(i =>
            s"SoftMaxDegraded<=${i._1}" ->
              toInt(atLeastOneScoreModif && alwaysPreserveClassif && alwaysSoftMaxDegradedLeq(i._2))
          ).toMap
    }
  }

  protected val scoreDegradationList = List(1E-3, 5E-3, 1E-2, 5E-2, 1E-1, 5E-1)
  protected val rankModifiedList = (0 to 10).toList
  protected val scoreModifiedList = (0 to 10).toList
  protected val softmaxDegradationList = List(1E-3, 5E-3, 1E-2, 5E-2, 1E-1, 5E-1)

  object ObservedMeasuresOld {

    def apply(x: CampaignResult): ObservedMeasuresOld = ObservedMeasuresOld(
      x.scores != x.goldScores,
      x.scores.keys.maxBy(x.scores) == x.goldScores.keys.maxBy(x.goldScores),
      scoreDegradationList.map(x.relativeVariation <= _),
      scoreModifiedList.map(x.scoreModified <= _),
      rankModifiedList.map(x.rankModified <= _),
      softmaxDegradationList.map(x.softmaxVariation <= _)
    )
  }

  case class MisclassificationExtendedMeasures(
                                                atLeastOneScoreModif: Boolean,
                                                atLeastOneMisclassification: Boolean,
                                                maxRankDowngrade: Int,
                                                alwaysTopScoreDegradedLeq: List[Boolean],
                                                alwaysScoreModifiedLeq: List[Boolean],
                                                alwaysRankModifiedLeq: List[Boolean],
                                                alwaysSoftMaxDegradedLeq: List[Boolean]) extends Measures {

    def combine(that: Measures): MisclassificationExtendedMeasures = that match {
      case t: MisclassificationExtendedMeasures => MisclassificationExtendedMeasures(
        atLeastOneScoreModif || t.atLeastOneScoreModif,
        atLeastOneMisclassification || t.atLeastOneMisclassification,
        maxRankDowngrade max t.maxRankDowngrade,
        alwaysTopScoreDegradedLeq.zip(t.alwaysTopScoreDegradedLeq).map(p => p._1 && p._2),
        alwaysScoreModifiedLeq.zip(t.alwaysScoreModifiedLeq).map(p => p._1 && p._2),
        alwaysRankModifiedLeq.zip(t.alwaysRankModifiedLeq).map(p => p._1 && p._2),
        alwaysSoftMaxDegradedLeq.zip(t.alwaysSoftMaxDegradedLeq).map(p => p._1 && p._2)
      )
      case _ => this
    }

    private def toInt(b: Boolean): Int = if (b) 1 else 0

    def toMap: Map[String, Int] = {
      Map(
        "1-Misc" -> toInt(atLeastOneMisclassification && maxRankDowngrade <= 1))
      scoreDegradationList
        .zipWithIndex
        .map(i =>
          s"TopScoreDegraded<=${i._1}" ->
            toInt(atLeastOneMisclassification && maxRankDowngrade <= 1 && alwaysTopScoreDegradedLeq(i._2))
        ).toMap ++
        scoreModifiedList
          .zipWithIndex
          .map(i =>
            s"scoreModified<=${i._1}" ->
              toInt(atLeastOneMisclassification && maxRankDowngrade <= 1 && alwaysScoreModifiedLeq(i._2))
          ).toMap ++
        rankModifiedList
          .zipWithIndex
          .map(i =>
            s"rankModified<=${i._1}" ->
              toInt(atLeastOneMisclassification && maxRankDowngrade <= 1 && alwaysRankModifiedLeq(i._2))
          ).toMap ++
        softmaxDegradationList
          .zipWithIndex
          .map(i =>
            s"SoftMaxDegraded<=${i._1}" ->
              toInt(atLeastOneMisclassification && maxRankDowngrade <= 1 && alwaysSoftMaxDegradedLeq(i._2))
          ).toMap
    }
  }

  object MisclassificationExtendedMeasures {
    def apply(x: CampaignResult): MisclassificationExtendedMeasures = {
      val observedClassification = x.scores.keys.maxBy(x.scores)
      val expectedClassification = x.goldScores.keys.maxBy(x.goldScores)
      MisclassificationExtendedMeasures(
        x.scores != x.goldScores,
        observedClassification != expectedClassification,
        x.scores.size - 1 - x.scores.keys.toSeq.sortBy(x.scores).indexOf(expectedClassification),
        scoreDegradationList.map(x.relativeVariation <= _),
        scoreModifiedList.map(x.scoreModified <= _),
        rankModifiedList.map(x.rankModified <= _),
        softmaxDegradationList.map(x.softmaxVariation <= _)
      )
    }
  }


  case class MisClassificationCountMeasures(
                                             countXMisc: List[Int],
                                             countMisc: Int,
                                             countObserved: Int,
                                             countMasked: Int,
                                           ) extends Measures {
    def combine(that: Measures): MisClassificationCountMeasures = that match {
      case t: MisClassificationCountMeasures =>
        MisClassificationCountMeasures(
          t.countXMisc.zip(countXMisc).map(p => p._2 + p._1),
          t.countMisc + countMisc,
          t.countObserved + countObserved,
          t.countMasked + countMasked)
      case _ => this
    }

    def toMap: Map[String, Int] =
      (1 to 9).map(i => s"$i-misc" -> countXMisc(i)).toMap +
        ("MisClassification" -> countMisc) +
        ("Observed" -> countObserved) +
        ("Masked" -> countMasked)
  }

  object MisClassificationCountMeasures {
    val measureNames: List[String] = "MisClassification" +: (1 to 9).map(i => s"$i-misc").toList

    def apply(x: CampaignResult): MisClassificationCountMeasures = {

      val rank = x.goldIndexInScores
      MisClassificationCountMeasures(
        if (x.isMisclassification) List.fill(10)(0).updated(rank, 1) else List.fill(10)(0),
        if (x.isMisclassification) 1 else 0,
        if (x.isObserved) 1 else 0,
        if (x.isMasked) 1 else 0
      )
    }
  }

  private def buildForall(campaignFiles: CampaignFiles, measure: CampaignResult => Measures) = {
    (for {
      result <- campaignFiles.resultsFiles.toList.sortBy(_._1.getParentFile.getName).flatMap(p => p._2.map(m => (p._1, m))).par
    } yield {
      val source = Source.fromFile(result._2)
      val injections = Parser.parse(Seq(source), result._1, campaignFiles.modelFile, campaignFiles.labelDictionary)
      val values = injections.map(x => measure(x)).toList
      source.close()
      values
    }).reduce((l, r) => l.zip(r).map(p => p._1 combine p._2))
      .map(_.toMap)
  }

  protected def buildCount(data: Range, campaignPath: String, excluding: Seq[String], measure: CampaignResult => Measures): Option[Map[String, Int]] = {
    for {
      campaignFiles <- getCampaignFiles(new File(campaignPath), MNIST, filter = data, exclude = Some(excluding))
    } yield {
      buildForall(campaignFiles, measure)
        .reduce((l, r) => l.transform((k, v) => r(k) + v))
    }
  }

  private def buildMap(campaignFiles: CampaignFiles, measure: CampaignResult => Measures) = {
    (for {
      result <- campaignFiles.resultsFiles.toList.sortBy(_._1.getParentFile.getName).flatMap(p => p._2.map(m => (p._1, m))).par
    } yield {
      val source = Source.fromFile(result._2)
      val injections = Parser.parse(Seq(source), result._1, campaignFiles.modelFile, campaignFiles.labelDictionary)
      val values = injections.map(x => (x.injectionPoint.toString, measure(x))).toMap
      source.close()
      values
    }).reduce((l, r) => l.map(p =>
      (p._1, p._2 combine r(p._1))
    ))
  }

  protected def buildCountMap(data: Range, campaignPath: String, excluding: Seq[String], measure: CampaignResult => Measures): Option[Map[String, Measures]] = {
    for {
      campaignFiles <- getCampaignFiles(new File(campaignPath), MNIST, filter = data, exclude = Some(excluding))
    } yield {
      buildMap(campaignFiles, measure)
    }
  }


  protected val toObservedMeasures: CampaignResult => ObservedMeasuresOld = x => ObservedMeasuresOld(x)

  protected val toMisClassificationMeasures: CampaignResult => MisClassificationMeasures = x => MisClassificationMeasures(x)

  protected val toMisclassificationExtendedMeasures: CampaignResult => MisclassificationExtendedMeasures = x => MisclassificationExtendedMeasures(x)
  protected val toMisClassificationCountMeasures: CampaignResult => MisClassificationCountMeasures = x => MisClassificationCountMeasures(x)

  val measureOrdering: Ordering[String] = Ordering.by(s => {
    val regexD = """([^0-9<=>!]*)([<=>!]*)([0-9]*(?>.[0-9]*))""".r
    val regex = """([^0-9]*)""".r
    val x = s match {
      case regex(h) => (h, 0.0)
      case regexD(h, _, d) => (h, if (d.nonEmpty) d.toDouble else 0.0)
      case _ => (s, 0.0)
    }
    x
  })

  protected def computeMeasure(measure: CampaignResult => Measures, resultFileName: String, fault: FaultType, fullTest: Boolean): Option[File] = {
    val range = if (fullTest) 0 until 10000 else 0 until 10
    val campaignPath = if (fullTest) "data/leNet5" else "src/test/resources/leNet5"
    val filters = fault match {
      case utils.FaultType.STUCK_AT_0 => Seq("bitflips", "stuck_at_1")
      case utils.FaultType.STUCK_AT_1 => Seq("bitflips", "stuck_at_0")
      case utils.FaultType.BITFLIP => Seq("stuck_at_0", "stuck_at_1")
      case _ => Seq("bitflips", "stuck_at_0", "stuck_at_1")
    }
    for {counts <- buildCount(range, campaignPath, filters, measure)
         resFile <- writeLogFile(resultFileName, Map("total" -> counts), "dummy" +: counts.keysIterator.toList.sorted(measureOrdering))
         } yield {
      resFile
    }
  }


  /**
   * Compute a map of measure (one line per injection)
   *
   * @param measure
   * @param resultFileName
   * @param fault    STUCK_AT_0/STUCK_AT_1 or BITFLIP
   * @param fullTest false: compute on test resources only/true: compute on all data
   */
  protected def computeMeasureMap(measure: CampaignResult => Measures, resultFileName: String, fault: FaultType, fullTest: Boolean): Unit = {
    val range = if (fullTest) 0 until 10000 else 0 until 10
    val campaignPath = if (fullTest) "data/leNet5" else "src/test/resources/leNet5"
    val filters = fault match {
      case utils.FaultType.STUCK_AT_0 => Seq("bitflips", "stuck_at_1")
      case utils.FaultType.STUCK_AT_1 => Seq("bitflips", "stuck_at_0")
      case utils.FaultType.BITFLIP => Seq("stuck_at_0", "stuck_at_1")
      case _ => Seq("bitflips", "stuck_at_0", "stuck_at_1")
    }
    for {counts <- buildCountMap(range, campaignPath, filters, measure)} {
      writeLogFile(resultFileName, counts.mapValues(_.toMap), "dummy" +: counts.head._2.toMap.keysIterator.toList.sorted(measureOrdering))
    }
  }
}