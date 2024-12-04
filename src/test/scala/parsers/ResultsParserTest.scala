package parsers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.FileManager

import java.io.File
import scala.io.Source

class ResultsParserTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {
  private def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  "A results parser" should "read result files" in {
    (for {
      resultsDir <- FileManager.extractResourceAsFile("leNet5/results/stuck_at_0")
      listOfResultsSources = getListOfFiles(resultsDir).filter(file => file.getName.contains("datalog")).map(Source.fromFile)
      campaignFile <- FileManager.extractResourceAsFile(s"leNet5/results/stuck_at_0/campaign.conf")
      modelFile <- FileManager.extractResourceAsFile("leNet5/architecture.json")
      labelFile <- FileManager.extractResourceAsFile("leNet5/labelsDictionary.txt")
    } yield {
      val parsingResults = Parser.parse(listOfResultsSources.take(10), new File(campaignFile), new File(modelFile), new File(labelFile))
      parsingResults.size shouldBe 12000
      listOfResultsSources.foreach(_.close())
    }) shouldBe defined
  }
}
