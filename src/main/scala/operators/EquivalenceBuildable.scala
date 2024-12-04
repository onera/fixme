package operators

import models.{CampaignResult, OrderEquivalenceClasses, ScoreEqualityEquivalenceClasses}


trait EquivalenceBuildable [T] {
  def buildFromClasses(classes: Stream[Stream[String]]): T
  def buildFromCampaign(campaignResult: Stream[CampaignResult]):T
}

object EquivalenceBuildable {
  val goldName = "golden"

  def buildEquivalenceClasses[A](results: Stream[CampaignResult], by: Map[String, Double] => A): Stream[Stream[String]] = {
    val goldScores = results.head.goldScores

    (results.map(c => by(c.scores) -> c.injectionPoint.toString) :+ (by(goldScores) -> goldName))
      .groupBy(_._1)
      .mapValues(_.map(_._2).toStream)
      .values
      .filter(_.size >= 2).toStream
  }

  trait Instances {


    implicit object ScoreEqualityEquivalenceClassesIsBuildable extends EquivalenceBuildable[ScoreEqualityEquivalenceClasses] {
      def buildFromClasses(classes: Stream[Stream[String]]): ScoreEqualityEquivalenceClasses = ScoreEqualityEquivalenceClasses(classes)

      def buildFromCampaign(campaignResult: Stream[CampaignResult]): ScoreEqualityEquivalenceClasses = {
        ScoreEqualityEquivalenceClasses(
          buildEquivalenceClasses(campaignResult, x => x))
      }
    }

    implicit object OrderEquivalenceClassesIsBuildable extends EquivalenceBuildable[OrderEquivalenceClasses] {
      def buildFromClasses(classes: Stream[Stream[String]]): OrderEquivalenceClasses = OrderEquivalenceClasses(classes)

      def buildFromCampaign(campaignResult: Stream[CampaignResult]): OrderEquivalenceClasses = OrderEquivalenceClasses(
        buildEquivalenceClasses(campaignResult, x => x.keys.toList.sortBy(x))) //warning: add support for multiple max
    }
  }
}
