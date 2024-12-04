package models

import indicators.FailureClass
import utils.Image

trait Result {
  val injectionPoint: Injection
  val activation: Image
}

object Result

case class RawCampaignResult(injectionPoint: Injection, activation: Image, scores: Map[String, Double]) extends Result

/**
 * Case class of campaign results to replace old versions of results
 *
 * @param injectionPoint
 * @param activation
 * @param scores
 * @param goldScores
 */
case class CampaignResult(injectionPoint: Injection, activation: Image, scores: Map[String, Double], goldScores: Map[String, Double]) extends Result {
  def toCSV: CampaignResultCSV = {
    CampaignResultCSV(activation.index, activation.label, injectionPoint.toString, scores, goldScores)
  }

  lazy val goldClassification: Set[String] = goldScores.groupBy(_._2).maxBy(_._1)._2.keySet

  lazy val observedClassification: Set[String] = scores.groupBy(_._2).maxBy(_._1)._2.keySet

  def isMisclassification(x: Int): Boolean = {
    goldIndexInScores >= x
  }

  lazy val isMisclassification: Boolean = goldClassification != observedClassification
  lazy val isMasked: Boolean = goldScores == scores

  lazy val isObserved: Boolean = !isMasked && goldClassification == observedClassification

  lazy val corruptionRatio: Double = {
    val corruptedMaxScore = scores.maxBy(_._2)._2
    val goldMaxScore = goldScores.maxBy(_._2)._2
    ((corruptedMaxScore - goldMaxScore) / goldMaxScore).abs
  }

  def isDegraded(ratio: Double): Boolean = {
    isObserved && corruptionRatio >= ratio
  }

  lazy val goldIndexInScores: Int = scores.size - 1 - scores.keys.toSeq.sortBy(scores).indexOf(goldClassification.head) //scores.toList.sortBy(_._2)(Ordering[Double].reverse).map(_._1).indexOf(goldClassification.head)

}
case class CampaignResultCSV(dataIndex: Int, dataLabel: String, injectionPoint: String, scores: Map[String, Double], goldScores: Map[String, Double])

case class AnalyzedResult(injectionPoint: Injection, activation: Image, failureClass: FailureClass)

case class ClassifiedResult(scenario: String, classes: List[String])