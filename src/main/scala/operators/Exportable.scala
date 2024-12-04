package operators

import exporters.FileWriter.{writeCsvFile, writeLogClassesFile, writeLogClassesMapFile, writeLogFile}
import models.Measures.{dataMeasureOrdering, measureOrdering}
import models._
import utils.UnionFind

import java.io.File

trait Exportable[T] {
  def export(name: String, data: T): Option[File]
}


object Exportable {
  private def toInt(b: Boolean): Int = if (b) 1 else 0
  trait Instances {

    implicit class ParsableOps(name: String) {
      def export[T](data: T)(implicit e: Exportable[T]): Option[File] = e.export(name, data)
    }

    implicit def EquivalenceClassesIsExportable[T <: EquivalenceClasses]: Exportable[T] =
      (name: String, data: T) => writeLogClassesFile(name, data.classes)

    implicit def UnionFindIsExportable[T <: UnionFind[String]]: Exportable[T] =
      (name: String, data: T) => writeLogClassesFile(name, data.equivalenceClasses)

    implicit def EquivalenceClassesMapIsExportable[T <: EquivalenceClassesMap]: Exportable[T] =
      (name: String, data: T) => writeLogClassesMapFile(name, data.classes)

    implicit object GlobalMeasuresStreamIsExportable extends Exportable[Stream[GlobalMeasures]] {
      def export(name: String, data: Stream[GlobalMeasures]): Option[File] = {
        val content = data.zipWithIndex.map(d => d._2 -> {
          import GlobalMeasures._
          import d._1._
          Map("atLeastOneScoreModif" -> toInt(atLeastOneScoreModif),
            "alwaysPreserveClassif" -> toInt(alwaysPreserveClassif),
            "maxRankDowngrade" -> maxRankDowngrade) ++
            scoreDegradationList
              .zipWithIndex
              .map(i =>
                s"alwaysTopScoreDegradedLeq${i._1}" ->
                  toInt(alwaysTopScoreDegradedLeq(i._2))
              ).toMap ++
          scoreModifiedList
            .zipWithIndex
            .map(i =>
              s"alwaysScoreModifiedLeq${i._1}" ->
                toInt(alwaysScoreModifiedLeq(i._2))
            ).toMap ++
          rankModifiedList
            .zipWithIndex
            .map(i =>
              s"alwaysRankModifiedLeq${i._1}" ->
                toInt(alwaysRankModifiedLeq(i._2))
            ).toMap ++
          softmaxDegradationList
            .zipWithIndex
            .map(i =>
              s"alwaysSoftMaxDegradedLeq${i._1}" ->
                toInt(alwaysSoftMaxDegradedLeq(i._2))
            ).toMap
        }).toMap
        writeLogFile(name, content, List("injection") ++ content.head._2.keys)
      }

    }

    implicit object ObservedMetricsIsExportable extends Exportable[ObservedMetrics] {
      def export(name: String, data: ObservedMetrics): Option[File] = {
        import GlobalMeasures._
        val toMap = (r: GlobalMeasures) => {
          import r._

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
        val counts = data.globalMeasures.map(toMap).reduce((l, r) => l.transform((k, v) => r(k) + v))
        writeCsvFile("observedMetrics" + name, Map("total" -> counts), "dummy" +: counts.keysIterator.toList.sorted(measureOrdering))
      }
    }

    implicit object MisClassificationMetricsIsExportable extends Exportable[MisClassificationMetrics] {
      def export(name: String, data: MisClassificationMetrics): Option[File] = {
        import GlobalMeasures._
        val toMap = (r: GlobalMeasures) => {
          import r._

          Map("1-Misc" -> toInt(!alwaysPreserveClassif && maxRankDowngrade == 1)) ++
            scoreDegradationList
              .zipWithIndex
              .map(i =>
                s"TopScoreDegraded<=${i._1}" ->
                  toInt(!alwaysPreserveClassif && maxRankDowngrade == 1 && alwaysTopScoreDegradedLeq(i._2))
              ).toMap ++
            scoreModifiedList
              .zipWithIndex
              .map(i =>
                s"scoreModified<=${i._1}" ->
                  toInt(!alwaysPreserveClassif && maxRankDowngrade == 1 && alwaysScoreModifiedLeq(i._2))
              ).toMap ++
            rankModifiedList
              .zipWithIndex
              .map(i =>
                s"rankModified<=${i._1}" ->
                  toInt(!alwaysPreserveClassif && maxRankDowngrade == 1 && alwaysRankModifiedLeq(i._2))
              ).toMap ++
            softmaxDegradationList
              .zipWithIndex
              .map(i =>
                s"SoftMaxDegraded<=${i._1}" ->
                  toInt(!alwaysPreserveClassif && maxRankDowngrade == 1 && alwaysSoftMaxDegradedLeq(i._2))
              ).toMap
        }
        val counts = data.globalMeasures.map(toMap).reduce((l, r) => l.transform((k, v) => r(k) + v))
        writeCsvFile("misClassificationMetrics" + name, Map("total" -> counts), "dummy" +: counts.keysIterator.toList.sorted(measureOrdering))
      }
    }

    implicit object ImpactCountTotalMeasuresIsExportable extends Exportable[ImpactCountTotalMeasures] {
      def export(name: String, data: ImpactCountTotalMeasures): Option[File] = {
        val toMap = (r: GlobalMeasures) => {
          import r._
          Map("Masked" -> toInt(!atLeastOneScoreModif),
            "Observed" -> toInt(atLeastOneScoreModif && alwaysPreserveClassif),
            "MisClassification" -> toInt(!alwaysPreserveClassif)) ++
            (0 until 10).map(x => s"${x}-Misc" -> toInt(!alwaysPreserveClassif && maxRankDowngrade == x)).toMap
        }
        val counts = data.globalMeasures.map(toMap).reduce((l, r) => l.transform((k, v) => r(k) + v))
        writeCsvFile("impactCountTotal" + name, Map("total" -> counts), "dummy" +: counts.keysIterator.toList.sorted(measureOrdering))
      }
    }

    implicit object DataMeasuresStreamIsExportable extends Exportable[Stream[DataMeasures]] {
      def export(name: String, data: Stream[DataMeasures]): Option[File] = {
        val content = data.map(d => {
          import d._
          point ->
            ((0 to 9).map(i => s"$i-misc" -> countXMisc(i)).toMap +
              ("MisClassification" -> countMisc) +
              ("Observed" -> countObserved) +
              ("Masked" -> countMasked))
        }).toMap
        writeLogFile(name, content, List("injection") ++ content.head._2.keys.toList.sorted(dataMeasureOrdering))
      }
    }

    implicit object AccuracyMeasuresStreamIsExportable extends Exportable[Stream[AccuracyMeasures]] {
      def export(name: String, data: Stream[AccuracyMeasures]): Option[File] = {
        val content = data.map(d => {
          import d._
          point ->
            Map("countMissGold" -> countMissGold,
              "countMissFaulty" -> countMissFaulty,
              "countTotal" -> countTotal)
        }).toMap
        writeLogFile(name, content, List("injection") ++ content.head._2.keys.toList.sorted)
      }
    }


    implicit object InjectionMeasuresStreamIsExportable extends Exportable[Stream[InjectionMeasures]] {
      def export(name: String, data: Stream[InjectionMeasures]): Option[File] = {
        val content = data.map(d => {
          import d._
          d.data ->
            ((0 to 9).map(i => s"$i-misc" -> countXMisc(i)).toMap +
              ("MisClassification" -> countMisc) +
              ("Observed" -> countObserved) +
              ("Masked" -> countMasked))
        }).toMap
        writeLogFile(name, content, List("data") ++ content.head._2.keys.toList.sorted(dataMeasureOrdering))
      }
    }
  }
}

