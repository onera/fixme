package parsers

import models.CampaignResultGen.CampaignResultGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import parsers.FilterParser.filterParser
import utils.FaultType.{BITFLIP, STUCK_AT_0, STUCK_AT_1}

class FilterParserTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  "correct expression" should "parsed correctly" in {
    val predicate = fastparse.parse("(dataLabel=='1' && bit!=2) || bit==1", filterParser(_))
    predicate.isSuccess shouldBe true
  }
  "incorrect attribute" should "be caught as a Decoding failure" in {
    val predicate = fastparse.parse("(label=='1' && bit!=2) || bit==1", filterParser(_))
    predicate.isSuccess shouldBe false
  }
  "incorrect parenthesis matching" should "be caught as a Decoding failure" in {
    val predicate = fastparse.parse("(dataLabel=='1' && bit!=2 || bit==1", filterParser(_))
    predicate.isSuccess shouldBe false
  }
  "incorrect test operator" should "be caught as a Decoding failure" in {
    val predicate = fastparse.parse("(dataLabel='1' && bit=!2) || bit==1", filterParser(_))
    predicate.isSuccess shouldBe false
  }


  "FilterParser" should "parse a condition on fault type" in {
    val predicate = fastparse.parse("faultType==BITFLIP", filterParser(_))
    predicate.isSuccess shouldBe true

    implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

    forAll((Gen.listOfN(1, CampaignResultGen), "data"), minSuccessful(1000)) {
      (data) =>
        val sa0 = data.head.copy(injectionPoint = data.head.injectionPoint.copy(faultType = STUCK_AT_0))
        val sa1 = data.head.copy(injectionPoint = data.head.injectionPoint.copy(faultType = STUCK_AT_1))
        val bf = data.head.copy(injectionPoint = data.head.injectionPoint.copy(faultType = BITFLIP))
        predicate.get.value(sa0) shouldBe false
        predicate.get.value(sa1) shouldBe false
        predicate.get.value(bf) shouldBe true
    }
  }
}
