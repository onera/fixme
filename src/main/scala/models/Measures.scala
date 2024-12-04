package models


trait Measures

object Measures {
  def toInt(b: Boolean): Int = if (b) 1 else 0

  def toBoolean(int: Int): Boolean = int != 0


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
  val dataMeasureOrdering: Ordering[String] = Ordering.by(s => {
    val regex = """([0-9]*)[^0-9]*""".r
    val x = s match {
      case "injection" => 0
      case "Masked" => 1
      case "Observed" => 2
      case "MisClassification" => 3
      case regex(number) => 4 + number.toInt
      case _ => 20
    }
    x
  })
}

case class GlobalMeasures(
                           atLeastOneScoreModif: Boolean,
                           alwaysPreserveClassif: Boolean,
                           alwaysTopScoreDegradedLeq: List[Boolean],
                           alwaysScoreModifiedLeq: List[Boolean],
                           alwaysRankModifiedLeq: List[Boolean],
                           alwaysSoftMaxDegradedLeq: List[Boolean],
                           maxRankDowngrade: Int) extends Measures

object GlobalMeasures {
  val scoreDegradationList = List(1E-3, 5E-3, 1E-2, 5E-2, 1E-1, 5E-1)
  val rankModifiedList = (0 to 10).toList
  val scoreModifiedList = (0 to 10).toList
  val softmaxDegradationList = List(1E-3, 5E-3, 1E-2, 5E-2, 1E-1, 5E-1)
}

case class ObservedMetrics(globalMeasures: Stream[GlobalMeasures]) extends Measures

case class MisClassificationMetrics(globalMeasures: Stream[GlobalMeasures]) extends Measures

case class ImpactCountTotalMeasures(globalMeasures: Stream[GlobalMeasures]) extends Measures

case class DataMeasures(
                         point: String,
                         countXMisc: List[Int],
                         countMisc: Int,
                         countObserved: Int,
                         countMasked: Int,
                       ) extends Measures

case class AccuracyMeasures(
                             point: String,
                             countMissGold: Int,
                             countMissFaulty: Int,
                             countTotal: Int
                           ) extends Measures

case class InjectionMeasures(
                              data: String,
                              countXMisc: List[Int],
                              countMisc: Int,
                              countObserved: Int,
                              countMasked: Int,
                            ) extends Measures
