package utils

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.Softmax.softmax


class SoftmaxTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {
  "Softmax " should "return" in {
    val rawScores = List(-2.5546875, 1.69921875, 2.77734375, 0.9296875, -4.6171875, -1.890625, -8.11328125, 16.21484375, -4.1875, 1.95703125)
      .zipWithIndex
      .map(p => (p._2.toString, p._1))
      .toMap

    val scores = softmax(rawScores).toList.sortBy(_._1).map(p => (p._1, (p._2)))

    for {
      s <- scores
    } yield {
      print(s"$s\n")
    }
  }
  val ScoreGen: Gen[(String, Double)] = {
    for {
      label <- Gen.stringOfN(5, Gen.alphaNumChar)
      score <- Gen.chooseNum[Double](-25.0, 25.0)
    }
    yield {
      (label, score)
    }
  }
  val ScoreMapGen: Gen[Map[String, Double]] = {
    Gen.mapOfN[String, Double](10, ScoreGen)
  }

  "the sum of softmax results" should "be equal to 1 +-1E-9" in {
    forAll((ScoreMapGen, "score map"), minSuccessful(100)) {
      (rawResults: Map[String, Double]) =>
        val scores = rawResults.map(p => (p._1, p._2))
        val softScores = softmax(scores)
        val sum = softScores.foldLeft(0.0)(_ + _._2)
        ((1 - sum).abs < 1E-9) shouldBe true
    }
  }
}

