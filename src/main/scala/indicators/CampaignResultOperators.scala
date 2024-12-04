package indicators

import models.CampaignResult
import utils.Softmax.softmax

object CampaignResultOperators {
  implicit class CampaignResultFunctionalImpact(c: CampaignResult) {
    lazy val relativeVariation: Double = {
      val scoreObs = c.scores(c.goldClassification.head)
      val scoreExp = c.goldScores(c.goldClassification.head)
      Math.abs(scoreObs - scoreExp) / scoreExp
    }

    lazy val softmaxVariation: Double = {
      val scoreObs = softmax(c.scores)(c.goldClassification.head)
      val scoreExp = softmax(c.goldScores)(c.goldClassification.head)
      assert(scoreObs <= 1 && scoreObs >= 0 && scoreExp <= 1 && scoreExp >= 0)
      math.abs(scoreObs - scoreExp)
    }

    lazy val scoreModified: Int = c.scores.count(p => c.goldScores(p._1) != p._2)

    lazy val rankModified: Int = {
      val orderedObs = c.scores.keys.toList.sortBy(c.scores)
      val expectedExp = c.goldScores.keys.toList.sortBy(c.goldScores)
      orderedObs.zip(expectedExp).count(p => p._1 != p._2)
    }
  }


}






