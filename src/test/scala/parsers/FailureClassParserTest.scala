package parsers

import models.{CampaignResult, Convolution2D, Injection, Shape2D}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers.FailureClassParser._
import utils.Image
import utils.FaultType.STUCK_AT_1
import utils.Softmax.softmax

class FailureClassParserTest extends AnyFlatSpec with should.Matchers {

  it should "parse a condition on scores" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"),
      Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("'3'.observedScore=='3'.expectedScore+4", boolean(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    result(c) shouldBe true
  }

  it should "parse a condition on classification" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("expectedClassif==observedClassif", boolean(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    result(c) shouldBe true
  }

  it should "parse a complete expression" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("'4'.observedScore+4=='4'.expectedScore+4 && ('2'.expectedScore==1 || '3'.observedScore==12)", failureClassParser(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    result(c) shouldBe true
  }

  it should "parse a complete boolean expression" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("'4'.observedScore+4=='4'.expectedScore+4&&('2'.observedScore==1 || '3'.expectedScore==1)", booleanExpression(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    result(c) shouldBe true
  }

  it should "parse an arithmetic operation" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("('3'.observedScore+2)/4", arithExpr(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    print(result(c))
    result(c) shouldBe (c.scores("3") + 2) / 4.0
  }

  it should "parse a simple arithmetic operation" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("(4 + 3 + 3/2)*2", arithExpr(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    print(result(c))
    result(c) shouldBe (4 + 3 + 3 / 2.0) * 2
  }

  it should "parse an arithmetic operation with abs" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("|-5*2|", arithExpr(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    result(c) shouldBe 10.0
  }

  it should "parse a float condition" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("observedScores.max == 12", boolean(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    print(result(c))
    result(c) shouldBe c.scores.maxBy(_._2)._2 == 12.0
  }

  it should "parse a string condition" in {
    val c = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
      1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
    val t = fastparse.parse("observedScores.argmax == '3'", boolean(_))
    println(t)
    t.isSuccess shouldBe true
    val result = t.get.value
    print(result(c))
    result(c) shouldBe c.scores.maxBy(_._2)._1 == "3"
  }
  val sc1Test = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
    1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 4, 2 -> 1, 3 -> 3, 4 -> 2).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
  val sc3Test = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
    1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 4, 2 -> 3, 3 -> 1, 4 -> 2).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
  val sc2Test = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
    1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 4, 2 -> 3, 3 -> 2, 4 -> 1).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
  val maskedTest = CampaignResult(Injection(Convolution2D("conv", Shape2D(1, 1), Shape2D(1, 1), Shape2D(1, 1), true, 1),
    1, 1, STUCK_AT_1, 1, 2), Image(1, "1"), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))), Map(1 -> 6, 2 -> 3, 3 -> 8, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))
  val observedTest = sc3Test.copy(scores = Map(1 -> 5, 2 -> 1, 3 -> 12, 4 -> 7).map(p => (p._1.toString, (p._2 / 1.0))))

  it should "find the good index rank" in {
    val rankOf = fastparse.parse("expectedClassif.observedScore.rank", FailureClassParser.number(_))
    rankOf.isSuccess shouldBe true
    rankOf.get.value(sc1Test) shouldBe 1.0
    rankOf.get.value(sc2Test) shouldBe 2.0
    rankOf.get.value(sc3Test) shouldBe 3.0
  }
  it should "parse correctly the masked failure class" in {
    val failureClass = fastparse.parse("labels.forall(label.observedScore == label.expectedScore)", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe false
    failureClass.get.value(maskedTest) shouldBe true
  }
  it should "parse correctly the observed failure class" in {
    val failureClass = fastparse.parse("observedClassif == expectedClassif && labels.exists(label.observedScore != label.expectedScore)", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe true
    failureClass.get.value(maskedTest) shouldBe false
  }

  it should "parse correctly the misclassification failure class" in {
    val failureClass = fastparse.parse("observedClassif != expectedClassif", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe true
    failureClass.get.value(sc2Test) shouldBe true
    failureClass.get.value(sc1Test) shouldBe true
    failureClass.get.value(observedTest) shouldBe false
    failureClass.get.value(maskedTest) shouldBe false
  }

  it should "parse correctly the SC-2 indicator" in {
    val failureClass = fastparse.parse("observedClassif != expectedClassif && expectedClassif.observedScore.rank >= 2", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe true
    failureClass.get.value(sc2Test) shouldBe true
    failureClass.get.value(sc1Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe false
    failureClass.get.value(maskedTest) shouldBe false
  }

  it should "parse correctly the 2-Misclassification indicator" in {
    val failureClass = fastparse.parse("observedClassif != expectedClassif && expectedClassif.observedScore.rank == 2", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe true
    failureClass.get.value(sc1Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe false
    failureClass.get.value(maskedTest) shouldBe false
  }
  it should "parse correctly the top score degradation indicator" in {
    val failureClass = fastparse.parse("observedClassif == expectedClassif && |(observedClassif.observedScore-expectedClassif.expectedScore)/expectedClassif.expectedScore| >= 0.5", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe true
    failureClass.get.value(maskedTest) shouldBe false
  }

  it should "parse correctly softmax" in {
    val failureClass = fastparse.parse("observedClassif.observedScore.softmax-expectedClassif.expectedScore.softmax", arithExpr(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(observedTest) shouldBe (softmax(observedTest.scores)(observedTest.observedClassification.head) - softmax(observedTest.goldScores)(observedTest.goldClassification.head))
  }

  it should "parse correctly softmax degradation indicator" in {
    val failureClass = fastparse.parse("observedClassif== expectedClassif && observedClassif.observedScore.softmax-expectedClassif.expectedScore.softmax >= 0", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe true
    failureClass.get.value(maskedTest) shouldBe true
  }


  it should "parse the count function" in {
    val failureClass = fastparse.parse("labels.count(label.observedScore > 3)", FailureClassParser.number(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(maskedTest) shouldBe 3.0
  }


  it should "parse correctly the max of softmax of scores with a one line comment" in {
    val failureClass = fastparse.parse("expectedClassif==observedClassif && expectedClassif.expectedScore.softmax-observedClassif.observedScore.softmax <= 0.4 //This is a comment", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe true
    failureClass.get.value(maskedTest) shouldBe true
  }
  it should "parse correctly the max of softmax of scores with a multi lines comment" in {
    val failureClass = fastparse.parse("expectedClassif==observedClassif && expectedClassif.expectedScore.softmax-observedClassif.observedScore.softmax <= 0.4 /* This is a comment */ ", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe true
    failureClass.get.value(maskedTest) shouldBe true
  }

  it should "parse the indicator: number of scores modified by a fault" in {
    val failureClass = fastparse.parse("observedClassif==expectedClassif && labels.count(label.observedScore!=label.expectedScore) == 3", failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(sc2Test) shouldBe false
    failureClass.get.value(sc1Test) shouldBe false
    failureClass.get.value(observedTest) shouldBe true
    failureClass.get.value(maskedTest) shouldBe false
  }

  "number parser" should "parse the function that gets the value of the 2nd ranked score" in {
    val failureClass = fastparse.parse("observedScores(1)", number(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe sc3Test.scores.toList.sortBy(_._2).reverse(1)._2
  }

  "failureClassParser" should "parse a condition on 1st ranked score and 2nd ranked score" in {
    val failureClass = fastparse.parse("observedScores(0)-observedScores(1)", arithExpr(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe sc3Test.scores.toList.sortBy(_._2).reverse.head._2 - sc3Test.scores.toList.sortBy(_._2).reverse.tail.head._2
  }

  "failureClassParserBis" should "able to parse the SC-3 definition" in {
    val failureClass = fastparse.parse("expectedClassif.observedScore.rank>=2", FailureClassParser.failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe true
  }
  "failureClassParserBis" should "parse the top degraded definition" in {
    val failureClass = fastparse.parse("expectedClassif==observedClassif && |(expectedClassif.expectedScore-observedClassif.observedScore)/expectedClassif.expectedScore|<=0.8", FailureClassParser.failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(observedTest) shouldBe true
  }

  "failureClassParserBis" should "parse the 2nd top degradation" in {
    val failureClass = fastparse.parse("expectedScores(0)", FailureClassParser.number(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe 8.0
  }

  it should "parse the misclassification with exists function" in {
    val failureClass = fastparse.parse("labels.exists(label.observedScore!=label.expectedScore)", FailureClassParser.failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe true
  }

  it should "parse the misclassification with forall function" in {
    val failureClass = fastparse.parse("labels.forall(label.observedScore==label.expectedScore)", FailureClassParser.failureClassParser(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe false
    failureClass.get.value(maskedTest) shouldBe true

  }

  it should "parse count function" in {
    val failureClass = fastparse.parse("labels.count(label.observedScore==label.expectedScore)", FailureClassParser.number(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe 1.0
    failureClass.get.value(maskedTest) shouldBe 4.0

  }

  it should "parse the count of modified rank" in {
    val failureClass = fastparse.parse("labels.count(label.observedScore.rank!=label.expectedScore.rank)", FailureClassParser.number(_))
    failureClass.isSuccess shouldBe true
    failureClass.get.value(sc3Test) shouldBe 4.0
    failureClass.get.value(observedTest) shouldBe 0.0
    failureClass.get.value(maskedTest) shouldBe 0.0

  }
  //
  //  val labels: CampaignResult => List[String] = ???
  //
  //  case class LabelScore(label: CampaignResult => String, isGolden: Boolean = false) {
  //    def apply(): CampaignResult => Double = (c: CampaignResult) => (if (isGolden) c.goldScores else c.scores) (label(c))
  //
  //    def rank: CampaignResult => Int = (c: CampaignResult) => (if (isGolden) c.goldScores else c.scores).toList.sortBy(_._2).reverse.indexWhere(_._1.matches(label(c)))
  //  }
  //
  //  implicit class labelOps(x: CampaignResult => String) {
  //    val observedScore = LabelScore(x)
  //    val expectedScore = LabelScore(x, isGolden = true)
  //  }
  //
  //  it should "parse the count of modified rank" in {
  //    val failureClass = fastparse.parse("labels.count(label.observedScore.rank!=label.expectedScore.rank)", FailureClassParser.failureClassParser(_))
  //    failureClass.isSuccess shouldBe true
  //
  //    labels
  //
  //  }

  "ArithExpr" should "parse correctly the top degraded operation" in {
    val failureClass = fastparse.parse("|(observedClassif.observedScore - expectedClassif.expectedScore)*100/expectedClassif.expectedScore| >0.5", FailureClassParser.boolean(_))
    failureClass.isSuccess shouldBe true
    val res = failureClass.get.value(observedTest)
    res shouldBe observedTest.corruptionRatio * 100 > 0.5
  }
  "ArithExpr" should "parse correctly the confidence degraded operation" in {
    val failureClass = fastparse.parse("(expectedClassif.expectedScore.softmax-observedClassif.observedScore.softmax)", FailureClassParser.arithExpr(_))
    failureClass.isSuccess shouldBe true
    val res = failureClass.get.value(observedTest)
    val so = (softmax(observedTest.scores), softmax(observedTest.goldScores))
    println(res)
  }
}
