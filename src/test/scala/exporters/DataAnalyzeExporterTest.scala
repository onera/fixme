package exporters


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers.MeasureResultsParser
import parsers.Parser.getCampaign
import utils.Dataset.MNIST
import utils.FaultType.STUCK_AT_0
import utils.FileManager.{extractResourceAsFile, getCampaignFiles, getExistingCsvFile}

import java.io.File


class DataAnalyzeExporterTest extends AnyFlatSpec with should.Matchers {

  "DataAnalyze" should "get the images misclassified for a given injection point" in {
    for {
      campaignFiles <- getCampaignFiles(new File("data/leNet5"), MNIST)
      campaign <- getCampaign(campaignFiles)
      ip <- campaign.strategy(STUCK_AT_0).find(_._2.toString.matches("conv2d.0.2:STUCK_AT_0@0"))
    } {
      val res = campaign(ip._2)
      val defined = res.collect {
        case Some(r) => r
      }
      val misclassifiedImages = defined.filter(_.isMisclassification)
      println(s"${misclassifiedImages.size}")
      println(s"${misclassifiedImages.groupBy(_.activation).keys}")
    }
  }

  private def getDataframe = for {
    fileSA0 <- getExistingCsvFile(s"ETSMisClassificationMeasuresMapSA0")
    fileSA1 <- getExistingCsvFile(s"ETSMisClassificationMeasuresMapSA1")
  } yield {
    MeasureResultsParser.parseFaultImpactCheckPoints(fileSA1, fileSA0)
  }

  "DataAnalyze" should "get the injection points where a given data is misclassified" in {
    for {
      campaignDir <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(campaignDir), MNIST)
      campaign <- getCampaign(campaignFiles)
      data <- campaign.dataSet.find(_.index == 1)
      res <- campaign(data)
    } {
      val misclassificationPoints = res.filter(_.isMisclassification)
      println(s"${misclassificationPoints.size}")
    }
  }

}
