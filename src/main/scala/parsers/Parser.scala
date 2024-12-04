package parsers

import exporters.ForallDataNumericBasedEqualityExporter
import indicators.{Degraded, FailureClass, Masked, XMisclassification}
import models._
import org.apache.spark.scheduler.SchedulingMode.NONE
import org.apache.spark.sql.functions.{call_udf, col, input_file_name, typedlit}
import org.apache.spark.sql.types.{FloatType, IntegerType, StringType, StructType}
import org.apache.spark.sql.{DataFrame, SparkSession}
import utils.FaultType.{BITFLIP, FaultType, STUCK_AT_0, STUCK_AT_1}
import utils.{CampaignFiles, Image, FaultType, UnionFind}

import java.io.File
import java.nio.file.Paths
import scala.io.{BufferedSource, Source}

object Parser {

  private def resultSchema(labelCount: Int) = (0 until labelCount).foldLeft(
    new StructType()
      .add("id", IntegerType))(
    (acc, x) => acc.add(s"score$x", FloatType)
  )

  private val campaignSchema = new StructType()
    .add("layer", IntegerType)
    .add("channel", IntegerType)
    .add("bit", IntegerType)
    .add("type", StringType)
    .add("startTime", IntegerType)
    .add("stopTime", IntegerType)

  private val datasetSchema = new StructType()
    .add("index", IntegerType)
    .add("label", IntegerType)

  case class IndexedInjection(id: Int, injectionPoint: String)

  def sparkParse(file: Seq[File], strategy: Map[Int, Injection], model: ConvolutionalNeuralNetwork, labels: Map[Int, String]): DataFrame = {
    val spark = SparkSession.builder
      .master("local[*]")
      .appName("Parser")
      .getOrCreate()
    import spark.implicits._
    spark.udf.register("get_only_activation", (fullPath: String) => fullPath.split('/').last.split('.').head.dropWhile(!_.isDigit))
    val rawResults = spark
      .read
      .schema(resultSchema(labels.size))
      .csv(file.map(_.getAbsolutePath): _*)
      .na
      .drop()
      .withColumn("activation", call_udf("get_only_activation", input_file_name()))

    val strategyDF = strategy.map(p => IndexedInjection(p._1, p._2.toString)).toSeq.toDF
    val results = strategyDF.join(rawResults, "id").drop("id")
    val goldens = results.filter(p => p
      .getAs[String]("injectionPoint")
      .contains("NONE"))
      .withColumn("injectionPoint", typedlit("NONE"))
    //    val renamedGolden = labels
    //      .foldLeft(goldens){(acc,x) => acc.withColumnRenamed(s"score${x._2}",s"goldScore${x._2}")}
    //      .drop("injectionPoint")
    val injectionResults = results.filter(p => !p.getAs[String]("injectionPoint").contains("NONE"))
    //injectionResults.join(renamedGolden,"activation")
    injectionResults union goldens
  }

  //TODO: modifier le passage des configurations des sequencers (pour l'instant hardcoded !!)
  // TODO: modify return type into an option
  def parse(seqFile: Seq[BufferedSource], campaignFile: File, modelFile: File, labelFile: File): Iterator[CampaignResult] = {
    (for {model <- CNNJsonParser.parseModel(modelFile)
          physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
          campaign = parseCampaign(campaignFile, physicalModel)
          labelDictionary = parseLabelFile(labelFile)
          }
    yield {
      seqFile.iterator.flatMap(x => parse(x, campaign, labelDictionary))
    }).get //OrElse(Iterator.empty[(ExtendedResult,Option[Boolean])])
  }

  def parse(resultFiles: Seq[(File, BufferedSource)], modelFile: File, labelFile: File): Iterator[CampaignResult] = {
    (for {model <- CNNJsonParser.parseModel(modelFile)
          physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
          labelDictionary = parseLabelFile(labelFile)
          }
    yield {
      resultFiles.iterator.flatMap(x => parse(x._2, parseCampaign(x._1, physicalModel), labelDictionary))
    }).get //OrElse(Iterator.empty[(ExtendedResult,Option[Boolean])])
  }

  // TODO: modify return type into an option
  def parse(s: BufferedSource, campaign: Map[Int, Injection], labelDictionary: Map[Int, String]): Iterator[CampaignResult] = {
    val content = s.getLines()
      .drop(1)
    val imageIdx = content
      .next().split("=").last.toInt

    val imageLabel = content.next().split("=").last
    val allResults = content
      .dropWhile(p => p.contains("#") || p.isEmpty)
      .map(_.split(',').map(_.toDouble))
      .map(array => {
        val indexInFile = array.head.toInt
        if (campaign.contains(indexInFile)) {
          val injection = campaign(array.head.toInt)
          Some(RawCampaignResult(injection, Image(imageIdx, imageLabel), array.tail.zipWithIndex.map(x => (labelDictionary(x._2), x._1)).toMap))
        }
        else
          None
      }).collect {
      case Some(c) => c
    }
    val gold = allResults.next
    assert(gold.injectionPoint.faultType == FaultType.NONE)
    assert(allResults.hasNext)
    allResults
      .map(x =>
        CampaignResult(injectionPoint = x.injectionPoint, activation = x.activation, scores = x.scores, goldScores = gold.scores))
      .filter(_.injectionPoint.faultType != FaultType.NONE)
  }

  def parseInjection(s: BufferedSource, campaign: Map[Int, Injection], labelDictionary: Map[Int, String], injectionPoint: Injection): Option[CampaignResult] = {
    val content = s.getLines()
      .drop(1)
    val imageIdx = content
      .next().split("=").last.toInt

    val imageLabel = content.next().split("=").last
    val allResults = content
      .dropWhile(p => p.contains("#") || p.isEmpty)
      .map(_.split(',').map(_.toDouble))
      .map(array => {
        val injection = campaign(array.head.toInt)
        RawCampaignResult(injection, Image(imageIdx, imageLabel), array.tail.zipWithIndex.map(x => (labelDictionary(x._2), x._1)).toMap)
      })
    val gold = allResults.next
    require(gold.injectionPoint.faultType == FaultType.NONE)
    require(allResults.hasNext)
    val testResults = allResults
      .map(x =>
        CampaignResult(injectionPoint = x.injectionPoint, activation = x.activation, scores = x.scores, goldScores = gold.scores))
      .filter(_.injectionPoint.faultType != FaultType.NONE)
    testResults.find(_.injectionPoint == injectionPoint)
  }

  def classifyResult(campaignResult: CampaignResult): FailureClass = {
    if (campaignResult.isObserved)
      Degraded(campaignResult.corruptionRatio)
    else if (campaignResult.isMasked)
      Masked
    else
      XMisclassification(campaignResult.goldIndexInScores)
  }

  def analyzeResults(results: Stream[CampaignResult]): Stream[AnalyzedResult] = {
    results.map(
      x => AnalyzedResult(
        x.injectionPoint,
        x.activation,
        classifyResult(x)
      )
    )
  }

  //WARNING: we added +1 in layer count to add results from input layer injections +1 because injection are done on the input of the following layer
  def parseCampaign(campaignFile: File, model: ConvolutionalNeuralNetwork): Map[Int, Injection] = {
    val s = Source.fromFile(campaignFile)
    val content = s
      .getLines()
      .drop(2)
      .map(_.split(',').map(_.replace(" ", "")))
      .zipWithIndex
      .collect({
        case (Array(layer, channel, bit, ftype, start, stop), id) =>
          id -> Injection(model.layers(layer.toInt + 2), channel.toInt, bit.toInt, FaultType.withName(ftype), start.toInt, stop.toInt)
      })
      .toMap
    s.close()
    content
  }

  def retrieveTimeSpecs(model: ConvolutionalNeuralNetwork, campaign: Map[Int, Injection]): Map[Layer2D, List[Int]] = model.layers.map(layer =>
    (layer,
      campaign
        .filter(_._2.layerId.name == layer.name)
        .filter(_._2.bitIndex == 0)
        .filter(_._2.channelIndex == 0)
        .filter(_._2.faultType == BITFLIP)
        .map(_._2.startDate)
        .toList
        .sortBy(_.toInt))
  ).toMap


  def convertCampaignToDataFlowModel(model: ConvolutionalNeuralNetwork, campaign: Map[Int, Injection], instantsDictionary: Map[Layer2D, Map[Int, Int]]): Map[Int, Injection] = {
    campaign.map {
      case (id, Injection(layerId, channelIndex, bitIndex, BITFLIP, startDate, stopDate)) =>
        //        if (instantsDictionary(layerId).keys)
        (id, Injection(layerId, channelIndex, bitIndex, BITFLIP, instantsDictionary(layerId)(startDate), instantsDictionary(layerId)(startDate) + 1))
      case (k, v) => (k, v)
    }
  }

  def parseLabelFile(labelFile: File): Map[Int, String] = {
    val s = Source.fromFile(labelFile)
    val content = s
      .getLines()
      .zipWithIndex
      .map(x => (x._2, x._1))
      .toMap
    s.close()
    content
  }

  def parseFunctionalClassesLog(file: File): Map[String, Int] = {
    val s = Source.fromFile(file)
    val content = s
      .getLines()
      .filterNot(_.isEmpty)
      .drop(1)
      .map(p => (p.split(',').head, p.split(',').last.toDouble.toInt))
      .toMap
    s.close()
    content
  }

  def parseFunctionalClassesSet(file: File): Set[Set[String]] = {
    val s = Source.fromFile(file)
    val content = s
      .getLines()
      .filterNot(_.isEmpty)
      .map(p => (p.split(',').toSet))
      .toSet
    s.close()
    content
  }


  def parseFunctionalClassesFile(file: File): UnionFind[String] = {
    val s = Source.fromFile(file)
    val content = s
      .getLines()
      .filterNot(_.isEmpty)
      .map(p => {
        p.split(',').toStream
      })
    val unionFind = UnionFind.empty[String]
    for {
      c <- content
    } {
      if (c.size == 1)
        unionFind += c.head
      else {
        c.foreach(x => unionFind += x)
        unionFind.addEquivalenceClass(c)
      }
    }
    s.close()
    unionFind
  }

  def parseInjectionPoint(dataDir: String, activation: Int, injection: Injection) = {
    val directoryPath = Paths.get(dataDir, "results" ++ (injection.faultType match {
      case STUCK_AT_1 =>
        "stuck_at_1"
      case STUCK_AT_0 =>
        "stuck_at_0"
      case BITFLIP => "bitlips"
    }) ++
      (injection.layerId match {
        case Input2D(_, _, _) => "_inputLayer"
        case _ => ""
      }))
    val campaignFile = Paths.get(directoryPath.toAbsolutePath.toString, s"datalog_${activation}").toFile

  }

  type InjectionStrategyDictionary = Map[FaultType, Map[Int, Injection]]
  type ResultsFileMap = Map[FaultType, List[File]]

  def parseDatasetFile(file: File): List[Image] = {
    val s = Source.fromFile(file)
    val content = s.getLines()
      .drop(1)
      .map(_.split(','))
      .map(kv => Image(kv.head.toInt, kv.last))
      .toList
    s.close()
    content
  }

  def getCampaign(campaignFiles: CampaignFiles) = for {
    model <- CNNJsonParser.parseModel(modelFile = campaignFiles.modelFile)
    physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D) //.remove("input_1")
    labelDictionary = parseLabelFile(campaignFiles.labelDictionary)
    dataset = parseDatasetFile(campaignFiles.datasetLabelsFile)
    resultsTagged = campaignFiles.resultsFiles.map(p => (parseCampaign(p._1, physicalModel), p._2))
      .map(s => (s._1.filterNot(_._2.faultType == NONE).head._2.faultType, s._1, s._2))
    strategy = resultsTagged.map(p => p._1 -> p._2).toMap
    resultsFileMap = resultsTagged.map(p => p._1 -> p._3.toList).toMap
  } yield {
    Campaign(physicalModel, labelDictionary, dataset, strategy, resultsFileMap)
  }
}
