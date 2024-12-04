package parsers

import models.Injection
import org.apache.spark
import org.apache.spark.sql.SparkSession
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import utils.FaultType
import utils.FileManager.{extractResourceAsFile, getExistingCsvFile}

import java.io.File

class MeasureResultsParserTest extends AnyFlatSpec with should.Matchers {
  it should "parse a measure result" in {
    val measureFile = new File("experimentalResults/ETSMisClassificationMeasuresMapSA1.csv")
    val dataframe = MeasureResultsParser.parse(measureFile)
    dataframe.show()
  }
  it should "parse a measure result and write it in a database" in {
    val measureFile = new File("experimentalResults/ETSMisClassificationMeasuresMapSA1.csv")
    val dataframe = MeasureResultsParser.parseFaultImpactCheckPoints(measureFile)
    dataframe.write.save("output/logs/testMeasureDB.parquet")
  }

  it should "load the test database into a dataframe" in {
    val spark = SparkSession.builder
      .master("local[*]")
      .appName("TestDatabaseRead")
      .getOrCreate()
    val dataframe = spark.read.load("output/logs/testMeasureDB.parquet")
dataframe.show()
  }
  it should "merge fault impact files correctly into a spark dataframe" in {
    for {
      files <- (0 until 10).toArray.map(i => getExistingCsvFile(s"faultImpact-${i}"))
        .foldLeft[Option[Seq[File]]](Some(Seq.empty[File]))((a, b) => if (a.isEmpty || b.isEmpty) None; else Some(a.get :+ b.get))
      modelFile <- extractResourceAsFile("leNet5/architecture.json")
      model <- parsers.CNNJsonParser.parseModel(new File(modelFile))
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
    } yield {
      val dataframe = MeasureResultsParser.parseFaultImpactCheckPoints(files: _*)
      dataframe.show()
      val res = dataframe.collect().map(row =>
        for {
          layer <- physicalModel.layers.find(_.name == row.getString(1))
        } yield {
          (Injection(layer,
            row.getInt(2),
            row.getInt(3),
            FaultType.withName(row.getString(4)),
            row.getInt(5),
            row.getInt(6))
            -> (7 until 18).map(i => row.getInt(i)))
        }
      )
      val merged = res.filter(_.isDefined).map(_.get).groupBy(_._1).map(r => (r._1, r._2.map(_._2).reduce(_.zip(_).map(e => e._1 + e._2))))
      merged
    }

  }

  it should "parse a fault impact file correctly into a spark dataframe" in {
    for {
      file <- getExistingCsvFile(s"faultImpactSAMerged")
      modelFile <- extractResourceAsFile("leNet5/architecture.json")
      model <- parsers.CNNJsonParser.parseModel(new File(modelFile))
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
    } yield {
      val dataframe = MeasureResultsParser.parseFaultImpactCheckPoints(file)
      dataframe.show()
    }
  }
}

class MeasureResultsExtractSAPointsTest extends AnyFlatSpec with should.Matchers {
  private def getDataframe = for {
    fileSA0 <- getExistingCsvFile(s"ETSMisClassificationMeasuresMapSA0")
    fileSA1 <- getExistingCsvFile(s"ETSMisClassificationMeasuresMapSA1")
  } yield {
    MeasureResultsParser.parseFaultImpactCheckPoints(fileSA1, fileSA0)
  }

  it should "parse a fault impact file and get stuck at injection points for which only 1 image is misclassified" in {
    for {
      dataframe <- getDataframe
    } yield {
      val filterdDataFrame = dataframe.filter(_.getAs[Int]("MisClassification") == 1)
      println(s"Number of injections with 1 misclassified image: ${filterdDataFrame.count}")
      filterdDataFrame.foreach { p => println(p.getAs[String]("dummy")) }
    }
  }

  it should "parse a fault impact file and get injection points for which 10 +/-3 images are misclassified" in {
    for {
      dataframe <- getDataframe
    } yield {
      val leq10miscl = dataframe.filter(p => {
        val e = p.getAs[Int]("MisClassification")
        e >= 7 && e <= 13
      })
      println(s"Number of injections with ~10 (+/-3) misclassified image: ${leq10miscl.count}")
      leq10miscl.foreach { p => println(p.getAs[String]("dummy")) }
    }
  }

  it should "parse a fault impact file and get the injections for which 50% (+/- 5) of images are misclassified" in {
    for {
      dataframe <- getDataframe
    } yield {
      val misclassif50percent = dataframe.filter(p => {
        val e = p.getAs[Int]("MisClassification")
        e >= 4500 && e <= 5500
      })
      println(s"Number of injections with ~50% (+-5%) misclassified image: ${misclassif50percent.count}")
      misclassif50percent.foreach { p => println(p.getAs[String]("dummy")) }
    }
  }

  it should "parse a fault impact file and get the injections for which more than 5500 images are misclassified" in {
    for {
      dataframe <- getDataframe
    } yield {
      val misclassifgt50percent = dataframe.filter(p => {
        val e = p.getAs[Int]("MisClassification")
        e >= 5500
      })
      println(s"Number of injections with more than 5500 misclassified images: ${misclassifgt50percent.count}")
      misclassifgt50percent.foreach { p => println(p.getAs[String]("dummy")) }
    }
  }
}
