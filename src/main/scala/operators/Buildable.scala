package operators

import indicators.CampaignResultOperators.CampaignResultFunctionalImpact
import models.{AccuracyMeasures, CampaignResult, DataMeasures, GlobalMeasures, InjectionMeasures, ScoreEqualityEquivalenceClassesMap}

trait Buildable[T] {
  def buildFrom(c: Stream[CampaignResult]): T
}

object Buildable {

  trait Instances {
    implicit def EquivalenceClassesIsBuildable[T](implicit equivalenceBuildable: EquivalenceBuildable[T]): Buildable[T] =
      (c: Stream[CampaignResult]) => equivalenceBuildable.buildFromCampaign(c)


    implicit object ScoreEqualityEquivalenceClassesMapIsBuildable extends Buildable[ScoreEqualityEquivalenceClassesMap] {
      def buildFrom(c: Stream[CampaignResult]): ScoreEqualityEquivalenceClassesMap = {
        val goldName = "golden"
        val goldScores = c.head.goldScores
        val t = ScoreEqualityEquivalenceClassesMap((c.map(p => p.scores -> p.injectionPoint.toString) :+ (goldScores -> goldName))
          .groupBy(_._1)
          .mapValues(_.map(_._2).toSeq)
          .values
          .filter(_.size >= 2)
          .flatMap(g => g.map(s => (s -> g))).toMap)
        t
      }
    }


    implicit object GlobalMeasuresIsBuildable extends Buildable[Stream[GlobalMeasures]] {

      import GlobalMeasures._

      def buildFrom(c: Stream[CampaignResult]): Stream[GlobalMeasures] = c.map(x =>
        GlobalMeasures(
          x.scores != x.goldScores,
          x.observedClassification == x.goldClassification,
          scoreDegradationList.map(x.relativeVariation <= _),
          scoreModifiedList.map(x.scoreModified <= _),
          rankModifiedList.map(x.rankModified <= _),
          softmaxDegradationList.map(x.softmaxVariation <= _),
          x.scores.size - 1 - x.scores.keys.toSeq.sortBy(x.scores).indexOf(x.goldClassification.head)
        )
      )
    }


    implicit object DataMeasuresIsBuildable extends Buildable[Stream[DataMeasures]] {
      def buildFrom(c: Stream[CampaignResult]): Stream[DataMeasures] = {
        import models.Measures._
        c.map(r =>
          DataMeasures(
            r.injectionPoint.toString,
            (0 to 9).map(p => toInt(r.isMisclassification && r.goldIndexInScores == p)).toList,
            toInt(r.isMisclassification),
            toInt(r.isObserved),
            toInt(r.isMasked)
          ))
      }
    }

    implicit object InjectionMeasuresIsBuildable extends Buildable[Stream[InjectionMeasures]] {
      def buildFrom(c: Stream[CampaignResult]): Stream[InjectionMeasures] = {
        Stream(InjectionMeasures(
          c.head.activation.index.toString,
          (0 to 9).map(p => c.count(r => r.isMisclassification && r.goldIndexInScores == p)).toList,
          c.count(_.isMisclassification),
          c.count(_.isObserved),
          c.count(_.isMasked)
        ))
      }
    }

    implicit object AccuracyMeasuresIsBuildable extends Buildable[Stream[AccuracyMeasures]] {
      def buildFrom(c: Stream[CampaignResult]): Stream[AccuracyMeasures] = {
        import models.Measures._
        c.map(p => AccuracyMeasures(
          p.injectionPoint.toString,
          toInt(p.activation.label != p.goldClassification.head && p.goldClassification.size == 1),
          toInt(p.activation.label != p.observedClassification.head && p.observedClassification.size == 1),
          1
        ))
      }
    }
  }
}