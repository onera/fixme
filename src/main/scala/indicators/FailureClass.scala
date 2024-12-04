package indicators

import indicators.Predicate.Predicate
import models.CampaignResult


trait FailureClass {
  def apply(p:CampaignResult): Boolean = predicate(p)
  val name: String
  val predicate: Predicate
}

object FailureClass {
  type FailureClasses = List[FailureClass]
}

case class XMisclassification(x: Int) extends FailureClass {
  val name = s"SC-$x"
  val predicate: Predicate = (p: CampaignResult) => p.scores.toList.sortBy(_._2)(Ordering[Double].reverse).map(_._1).indexOf(p.goldClassification) == x
}


object Masked extends FailureClass {
  val name = "Masked"
  val predicate: Predicate = (p: CampaignResult) => p.goldScores == p.scores
}

trait Observed extends FailureClass
object Observed extends FailureClass {
  val name = "Observed"
  val predicate: Predicate = (p: CampaignResult) => p.isObserved
}

case class Degraded(r: Double) extends Observed {
  val name = s"Degraded-$r"
  val predicate: Predicate = (p: CampaignResult) => p.isDegraded(r)
}

object GoldMisclassification extends FailureClass {
  val name = "Gold Misclassification"
  val predicate: Predicate = (p: CampaignResult) => p.goldClassification != p.activation.label.toString
}

object CorruptedMisclassification extends FailureClass {
  val name = "Corrupted Misclassification"
  val predicate: Predicate = (p: CampaignResult) => p.observedClassification != p.activation.label.toString
}

case class OtherFailureClass(name: String, predicate: Predicate) extends FailureClass