package indicators

import indicators.FailureClass.FailureClasses
import models.{CampaignResult, ClassifiedResult}

trait isMeasure {
  val name: String
  val failureClasses: FailureClasses
  val filter: (CampaignResult => Boolean)
  val projection: (CampaignResult => String)
}

object isMeasure

trait Measure[V] extends isMeasure {
  val metric: Metric[V]

  def apply(campaignResultStream: Stream[CampaignResult]): V = {
    metric(campaignResultStream
      .filter(filter)
      .map(p => ClassifiedResult(projection(p), failureClasses.map(fc => (fc.name, fc.predicate(p))).filter(_._2).map(_._1))))

  }

  //  def apply(campaignResultStreams: Seq[CampaignResults]): V = {
  //    metric(campaignResultStreams.par.map(campaignResults =>campaignResults
  //      .filter(filter)
  //      .map(p => ClassifiedResult(projection(p), failureClasses.map(fc => (fc.name, fc.predicate(p))).filter(_._2).map(_._1))))
  //   )
  //  }
}


case class IntMeasure(name: String, failureClasses: FailureClasses,
                      filter: (CampaignResult => Boolean),
                      projection: (CampaignResult => String),
                      metric: Metric[Int]) extends Measure[Int]


case class MapClassesMeasure(name: String, failureClasses: FailureClasses,
                             filter: (CampaignResult => Boolean),
                             projection: (CampaignResult => String),
                             metric: Metric[Map[String, Int]]) extends Measure[Map[String, Int]] {

}