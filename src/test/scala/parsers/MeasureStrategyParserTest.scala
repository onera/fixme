package parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.FileManager

import java.io.File

class MeasureStrategyParserTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {


  it should "parse a measure strategy definition" in {
    for {
      newMsFile <- FileManager.extractResourceAsFile("measures/ets23.json")
      fcFile <- FileManager.extractResourceAsFile("indicators/failureClassesCNN.json")
    } yield {
      val measureStrategy = MeasureStrategyParser.parseMeasureStrategy(new File(newMsFile), new File(fcFile))
      measureStrategy.toOption shouldBe defined
    }
  }

}
