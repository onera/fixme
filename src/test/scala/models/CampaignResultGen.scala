package models

import org.scalacheck.Gen
import utils.DatasetItemGen

object CampaignResultGen {
  /**
   * Define a way to generate random results
   */
  val CampaignResultGen: Gen[CampaignResult] = for {
    injectionPoint <- InjectionGen.injectionGen
    activation <- DatasetItemGen.datasetItemGen
    tupleScores <- ScoreMapGen
    scores <- tupleScores._2
    gold <- tupleScores._1
  } yield
    CampaignResult(injectionPoint, activation, scores, gold)

  /**
   * Define a way to generate a Score
   */
  val ScoreGen: Gen[(String, (Double, Double))] = {
    for {
      label <- Gen.stringOfN(5, Gen.alphaNumChar)
      gold <- Gen.chooseNum[Float](Char.MinValue, Char.MaxValue)
      result <- Gen.chooseNum[Float](Char.MinValue, Char.MaxValue)
    }
    yield {
      (label, (gold, result))
    }
  }
  /**
   * Define a way to generate a score map and a gold score map
   */
  val ScoreMapGen: Gen[(Gen[Map[String, Double]], Gen[Map[String, Double]])] = {
    for {
      score <- Gen.listOfN(10, ScoreGen)
    } yield {
      (score.toMap.map(p => (p._1, p._2._1)), score.toMap.map(p => (p._1, p._2._2)))
    }
  }

  /**
   * Define a way to generate random data
   */
  val dataGen: Gen[CampaignResult] = for {
    test <- CampaignResultGen
  } yield {
    test
  }


}
