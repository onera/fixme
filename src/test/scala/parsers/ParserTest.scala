package parsers

import models.LeNetFixture.VhdlImplementedLeNetFixture
import org.apache.spark.sql.SparkSession
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import parsers.Parser.{getCampaign, parseCampaign, retrieveTimeSpecs, sparkParse}
import utils.Dataset.MNIST
import utils.FaultType.{BITFLIP, STUCK_AT_0, STUCK_AT_1}
import utils.FileManager
import utils.FileManager.{extractResourceAsFile, getCampaignFiles}

import java.io.{File, FileWriter}
import scala.io.Source

class ParserTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  "A parser" should "read be able to retrieve times of activations of faults for LaeNet5" in {
    for {
      resultsDir <- FileManager.extractResourceAsFile("leNet5/results/bitflips")
      campaignFile <- FileManager.extractResourceAsFile("leNet5/results/bitflips/campaign.conf")
      modelFile <- FileManager.extractResourceAsFile("leNet5/architecture.json")
      model <- CNNJsonParser.parseModel(new File(modelFile))
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
      campaign = parseCampaign(new File(campaignFile), physicalModel)
    } yield {
      val timeSpecs = retrieveTimeSpecs(model, campaign).map(p => (p._1.name, p._2))
      timeSpecs.size shouldBe 7
      timeSpecs("conv2d").size shouldBe 0 //here no injection on input of 1st conv layer
      timeSpecs("max_pooling2d").size shouldBe 28 * 28
      timeSpecs("conv2d_1").size shouldBe 14 * 14
      timeSpecs("max_pooling2d_1").size shouldBe 10 * 10
      timeSpecs("dense").size shouldBe 5 * 5
      timeSpecs("dense_1").size shouldBe 12
      timeSpecs("dense_2").size shouldBe 4

    }
  }


  it should "check the consistency between the model instants and the strategy instants" in {
    for {
      resultsDir <- FileManager.extractResourceAsFile("leNet5/results/bitflips")
      campaignFile <- FileManager.extractResourceAsFile("leNet5/results/bitflips/campaign.conf")
      modelFile <- FileManager.extractResourceAsFile("leNet5/architecture.json")
      model <- CNNJsonParser.parseModel(new File(modelFile))
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
      campaignOthers = parseCampaign(new File(campaignFile), physicalModel)
      campaignInput = parseCampaign(new File("data/leNet5/results/bitflips_inputLayer/campaign.conf"), physicalModel)
      campaign = campaignInput ++ campaignOthers.map(p => (p._1 + campaignInput.size, p._2))
      input <- model.getLayerByName("conv2d")
      conv1 <- model.getLayerByName("max_pooling2d")
      pool1 <- model.getLayerByName("conv2d_1")
      conv2 <- model.getLayerByName("max_pooling2d_1")
      pool2 <- model.getLayerByName("dense")
      dense1 <- model.getLayerByName("dense_1")
      dense2 <- model.getLayerByName("dense_2")
    } yield {
      val timeSpecs = retrieveTimeSpecs(model, campaign)
      timeSpecs.size shouldBe 7
      val network = VhdlImplementedLeNetFixture

      timeSpecs(input).filterNot(_ > 1024).size shouldBe network.imageInput.toList.size
      timeSpecs(conv1).size shouldBe network.conv1Output.toList.size //.map(_+2)
      timeSpecs(pool1).size shouldBe network.pool1Output.toList.size //.map(_+4)
      network.conv2Output.toList.size shouldBe timeSpecs(conv2).size
      network.pool2Output.toList.size shouldBe timeSpecs(pool2).size
      network.seq1Output.toList.size shouldBe timeSpecs(dense1).size
      network.seq2Output.toList.size shouldBe timeSpecs(dense2).size
      val shiftInput = timeSpecs(input).filterNot(_ > 1024).zip(network.imageInput.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      val shiftConv1 = timeSpecs(conv1).zip(network.conv1Output.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      val shiftPool1 = timeSpecs(pool1).zip(network.pool1Output.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      val shiftConv2 = timeSpecs(conv2).zip(network.conv2Output.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      val shiftPool2 = timeSpecs(pool2).zip(network.pool2Output.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      val shiftDense1 = timeSpecs(dense1).zip(network.seq1Output.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      val shiftDense2 = timeSpecs(dense2).zip(network.seq2Output.toList).filter(p => p._1 != p._2) //.map(p => p._1 - p._2)
      timeSpecs(input).filterNot(_ > 1024) shouldBe network.imageInput.toList
      timeSpecs(conv1) shouldBe network.conv1Output.toList
      timeSpecs(pool1) shouldBe network.pool1Output.toList
      network.conv2Output.toList shouldBe timeSpecs(conv2)
      network.pool2Output.toList shouldBe timeSpecs(pool2)
      network.seq1Output.toList shouldBe timeSpecs(dense1)
      network.seq2Output.toList shouldBe timeSpecs(dense2)
    }
  }

  //  it should "create a dictionary for injection instants" in {
  //    for {
  //      resultsDir <- FileManager.extractResourceAsFile("leNet5/results/bitflips")
  //      campaignFile <- FileManager.extractResourceAsFile("leNet5/results/bitflips/campaign.conf")
  //      modelFile <- FileManager.extractResourceAsFile("leNet5/architecture.json")
  //      model <- CNNJsonParser.parseModel(new File(modelFile))
  //      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
  //      campaignOthers = parseCampaign(new File(campaignFile), physicalModel)
  //      campaignInput = parseCampaign(new File("data/leNet5/results/bitflips_inputLayer/campaign.conf"), physicalModel)
  //      campaign = campaignInput ++ campaignOthers.map(p => (p._1 + campaignInput.size, p._2))
  //      input <- model.getLayerByName("conv2d")
  //      conv1 <- model.getLayerByName("max_pooling2d")
  //      pool1 <- model.getLayerByName("conv2d_1")
  //      conv2 <- model.getLayerByName("max_pooling2d_1")
  //      pool2 <- model.getLayerByName("dense")
  //      dense1 <- model.getLayerByName("dense_1")
  //      dense2 <- model.getLayerByName("dense_2")
  //    } yield {
  //      val timeSpecs = retrieveTimeSpecs(model, campaign)
  //      timeSpecs.size shouldBe 7
  //      val network = VhdlImplementedLeNetFixture
  //      val dictionary = Map(
  //        input -> timeSpecs(input).filterNot(_ > 1024).sorted.zip(network.imageInput.toList.sorted).toMap,
  //        conv1 -> timeSpecs(conv1).sorted.zip(network.conv1Output.toList.sorted).toMap,
  //        pool1 -> timeSpecs(pool1).sorted.zip(network.pool1Output.toList.sorted).toMap,
  //        conv2 -> timeSpecs(conv2).sorted.zip(network.conv2Output.toList.sorted).toMap,
  //        pool2 -> timeSpecs(pool2).sorted.zip(network.pool1Output.toList.sorted).toMap,
  //        dense1 -> timeSpecs(dense1).sorted.zip(network.seq1Output.toList.sorted).toMap,
  //        dense2 -> timeSpecs(dense2).sorted.zip(network.seq2Output.toList.sorted).toMap
  //      )
  //      val filteredCampaign = campaign.filterNot(p => p._2.layerId == input && p._2.startDate > 1024)
  //      val convertedCampaign = convertCampaignToDataFlowModel(physicalModel, filteredCampaign, dictionary)
  //      val timeSpecsConverted = retrieveTimeSpecs(model, convertedCampaign)
  //    }
  //  }

  it should "provide 10 scores per injection for SA 0" in {
    for {
      resultFile <- FileManager.extractResourceAsFile("leNet5/results/stuck_at_0/datalog_0")
      campaignFile <- FileManager.extractResourceAsFile("leNet5/results/stuck_at_0/campaign.conf")
      modelFile <- FileManager.extractResourceAsFile("leNet5/architecture.json")
      labelFile <- FileManager.extractResourceAsFile("leNet5/labelsDictionary.txt")
      source = Source.fromFile(new File(resultFile))
      it = Parser.parse(Seq(source), new File(campaignFile), new File(modelFile), new File(labelFile))
    } yield {
      it.foreach(_.scores.size shouldBe 10)
      source.close()
    }
  }

  it should "parse th functional equivalence list" in {
    for {
      file <- extractResourceAsFile("classes/sa0FunctionallyEquivalent.csv")
    } yield {
      val t = Parser.parseFunctionalClassesLog(new File(file))
      val s = t.groupBy(_._2).mapValues(_.size)
      print(s)
    }
  }
}

class MergeResults extends AnyFlatSpec {
  def mergeRawCampaignFiles(file: File, addFile: File) = {
    val s1 = Source.fromFile(file)
    val lines = s1.getLines().toList.filter(_.nonEmpty)
    val s2 = Source.fromFile(addFile)
    val addLines = s2.getLines().filter(_.nonEmpty).drop(3).filterNot(_.contains("NONE")).toList
    val merged = (lines ++ addLines)
    s1.close()
    s2.close()
    merged
  }

  def mergeRawResultsFiles(file: File, addFile: File) = {
    val s1 = Source.fromFile(file)
    val lines = s1.getLines().toList.filter(_.nonEmpty)
    val s2 = Source.fromFile(addFile)
    val addLines = s2.getLines().filter(_.nonEmpty).drop(5).map(_.split(',').tail.mkString(","))
    val merged = (lines ++ addLines.filterNot(_.isEmpty).zipWithIndex.map(p => (lines.size - 4 + p._2).toString + "," + p._1)).toList.dropRight(1)
    s1.close()
    s2.close()
    merged
  }

  def mergeDirectories(srcDir: String, targetDir: String, exclude: Seq[String], drop: Int = 0) = {
    for {
      campaignFiles <- getCampaignFiles(new File(srcDir), MNIST, exclude = Some(exclude))
      model <- CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
    } yield {
      val directory = new File(targetDir)
      if (!directory.exists) directory.mkdir()
      val resultFiles = campaignFiles.resultsFiles
      val (firstC, lastC) = if (resultFiles.keys.head.getParent.contains("inputLayer"))
        (resultFiles.keys.head, resultFiles.keys.last)
      else
        (resultFiles.keys.last, resultFiles.keys.head)
      val campaigns = mergeRawCampaignFiles(firstC, lastC)
      val values = campaignFiles.resultsFiles.values.map(_.sortBy(_.getName)).reduce {
        _ ++ _
      }.groupBy(_.getName).toList
      val fwC = new FileWriter(new File(targetDir + "/" + "campaign.conf"))
      fwC.write(campaigns.mkString("\n"))
      fwC.close()

      for {
        (name, file1, file2) <- values.par.map(p => (p._1, p._2.head, p._2.last))
      } {
        val (headFile, addFile) = if (file1.getParent.contains("inputLayer")) (file1, file2) else (file2, file1)
        val merged = mergeRawResultsFiles(headFile, addFile).dropRight(drop)
        val fwR = new FileWriter(new File(targetDir + "/" + file2.getName))
        fwR.write(merged.mkString("\n"))
        fwR.close()
      }
    }
  }

  "Merge campaign data" should "merge the stuck_at 0 directories" in {
    mergeDirectories("data/leNet5", "data/leNet5/results/stuck_at_0_merged", Seq("bitflips", "stuck_at_1", "merged"))
  }

  "Merge campaign data" should "merge the stuck_at 1 directories" in {
    mergeDirectories("data/leNet5", "data/leNet5/results/stuck_at_1_merged", Seq("bitflips", "stuck_at_0", "merged"))
  }

  "Merge campaign data" should "merge the bitflips directories" in {
    mergeDirectories("data/leNet5", "data/leNet5/results/bitflips_merged", Seq("stuck_at", "merged"), 1)
  }

}

class CampaignParserTest extends AnyFlatSpec with should.Matchers {
  "getCampaign" should "return a campaign object ready to be used" in {
    for {
      campaignDir <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(campaignDir), MNIST)

    } yield {
      val campaignOpt = getCampaign(campaignFiles)
      campaignOpt shouldBe defined
      campaignOpt.get.dataSet
    }
  }

}

class CampaignResultsSparkParserTest extends AnyFlatSpec with should.Matchers {
  "A spark parser" should "read result SA0 files and store them in a database" in {
    (for {
      resultsDir <- FileManager.extractResourceAsFile("leNet5")
      campaignFiles <- FileManager.getCampaignFiles(new File(resultsDir), MNIST)
      campaign <- getCampaign(campaignFiles)
    } yield {
      val res = sparkParse(campaign.results(STUCK_AT_0), campaign.strategy(STUCK_AT_0), campaign.model, campaign.labels)
      res.show()
      res.write.mode("append").save("output/databases/campaignResultsSA0Test.parquet")
    }) shouldBe defined
  }
  it should "read result bit-flip files and store them in a database" in {
    (for {
      resultsDir <- FileManager.extractResourceAsFile("leNet5")
      campaignFiles <- FileManager.getCampaignFiles(new File(resultsDir), MNIST)
      campaign <- getCampaign(campaignFiles)
    } yield {
      val res = sparkParse(campaign.results(BITFLIP), campaign.strategy(BITFLIP), campaign.model, campaign.labels)
      res.show()
      res.write.mode("append").save("output/databases/campaignResultsBFTest.parquet")
    }) shouldBe defined
  }

  "the databases" should "be readable" in {
      val spark = SparkSession.builder
        .master("local[*]")
        .appName("Parser")
        .getOrCreate()
      val res = spark.read.load("output/databases/campaignResultsSA0Test.parquet")
      res.show()
  }
}
  class GlobalCampaignResultsSparkParserTest extends AnyFlatSpec with should.Matchers {

  "A spark parser" should "read all SA-0 result files and store them in a database" in {
    (for {
      campaignFiles <- FileManager.getCampaignFiles(new File("data/leNet5"), MNIST)
      campaign <- getCampaign(campaignFiles)
    } yield {
      val res = sparkParse(campaign.results(STUCK_AT_0), campaign.strategy(STUCK_AT_0), campaign.model, campaign.labels)
      res.write.save("output/databases/campaignResultsSA0.parquet")
    }) shouldBe defined
  }
    "A spark parser" should "read all SA-1 result files and store them in a database" in {
      (for {
        campaignFiles <- FileManager.getCampaignFiles(new File("data/leNet5"), MNIST)
        campaign <- getCampaign(campaignFiles)
      } yield {
        val res = sparkParse(campaign.results(STUCK_AT_1), campaign.strategy(STUCK_AT_1), campaign.model, campaign.labels)
        res.write.save("output/databases/campaignResultsSA1.parquet")
      }) shouldBe defined
    }
    "A spark parser" should "read all bitflips result files and store them in a database" in {
      (for {
        campaignFiles <- FileManager.getCampaignFiles(new File("data/leNet5"), MNIST)
        campaign <- getCampaign(campaignFiles)
      } yield {
        val res = sparkParse(campaign.results(BITFLIP), campaign.strategy(BITFLIP), campaign.model, campaign.labels)
        res.write.save("output/databases/campaignResultsBF.parquet")
      }) shouldBe defined
    }
}