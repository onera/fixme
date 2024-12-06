import CliMode.{CliMode, Coverage, EquivalenceClasses, FlowAnalysis, InjectionAnalysis}
import exporters.MeasureExporter
import models.InjectionStrategy
import operators.all._
import org.apache.parquet.internal.filter2.columnindex.ColumnIndexStore
import parsers.MeasureStrategyParser.parseMeasureStrategy
import parsers.Parser.{getCampaign, parseFunctionalClassesLog, retrieveTimeSpecs}
import parsers.{CNNJsonParser, Parser}
import scopt.OParser
import utils.Dataset.MNIST
import utils.FileManager

import java.io.File
import scala.io.Source

object CliMode extends Enumeration {
  protected case class Val(modeName: String) extends super.Val{}
  type CliMode = Value
  import scala.language.implicitConversions

  implicit def valueToFaultType(x: Value): Val = x.asInstanceOf[Val]
  val InjectionAnalysis = Val("injections")
  val FlowAnalysis = Val("flows")
  val Coverage = Val("coverage")
  val EquivalenceClasses = Val("classes")
}
case class Config(
                   campaignDir: File = new File(""),
                   verbose: Boolean = false,
                   datasetFilter: Range = 0 to Int.MaxValue,
                   measureStrategyFile: File = new File(""),
                   cnnModelFile: File = new File(""),
                   failureClassFile: File = new File(""),
                   globalClassFile: Option[String] = None,
                   diagramGenerationEnabled: Boolean = false,
                   mode: CliMode = InjectionAnalysis,
                   injectionStrategy : String = "",
                   command: String = "",
                   excludeString: Option[Seq[String]] = None,
                   measureStrategyInputEnabled: Boolean = false,
                   checkpointStep: Int = 0,
                   checkConsistency: Boolean = false,
                   overrideMsName: Option[String] = None,
                   threads: Int = Runtime.getRuntime.availableProcessors()
                 )


object main extends App {
  val builder = OParser.builder[Config]

  val parser1 = {
    import builder._
    def checkDirectory(f: File): Either[String, Unit] = {
      if (f.isDirectory) success
      else failure(s"$f is not a directory")
    }

    def checkFile(f: File): Either[String, Unit] = {
      if (f.isFile) success
      else failure(s"$f is not a valid file")
    }

    val regexDF = """\((\d*),(\d*)\)""".r
    OParser.sequence(
      programName("fixme"),
      head("fixme", "1.0"),
      opt[Unit]('v', "verbose")
        .optional()
        .action((_, c) => c.copy(verbose = true)),
      note("some notes." + sys.props("line.separator")),
      cmd("analysis")
        .action((_, c) => c.copy(mode = InjectionAnalysis))
        .children(
          opt[File]('M', "measures")
            .optional()
            .valueName("<MeasureStrategyFile>")
            .validate(f =>
              if (!f.exists()) failure(s"${f.getAbsolutePath} does not exist")
              else if (!f.isFile) failure(s"${f.getAbsolutePath} is not a file")
              else success
            )
            .action((x, c) => c.copy(measureStrategyFile = x, measureStrategyInputEnabled = true))
            .text("Measure strategy file contains several indicators to be applied to campaign results"),

          opt[Unit]("consistency")
            .abbr("cc")
            .optional()
            .action((_, c) => c.copy(checkConsistency = true)),

          opt[String]("overrideMsName")
            .abbr("msn")
            .optional()
            .action((x, c) => c.copy(overrideMsName = Some(x))),

          opt[File]("failureClasses")
            .abbr("fc")
            .valueName("<JSON>")
            .validate(checkFile)
            .action(
              (x, c) => c.copy(failureClassFile = x)
            )
            .text("Failure Classes to use with the measure strategy"),

          opt[Unit]("plot")
            .abbr("p")
            .action((_, c) => c.copy(diagramGenerationEnabled = true))
            .text("Enable plot generation"),
          opt[Int]("checkpointStep")
            .abbr("cps")
            .action((x, c) => c.copy(checkpointStep = x))
            .validate {
              x =>
                if (x <= 0)
                  failure(s"checkpoint step must be a strictly positive integer.")
                else
                  success
            }
            .text("the number of data to treat before saving a temporary csv measure file."),

          opt[Int]("threads")
            .abbr("th")
            .action((x, c) => c.copy(threads = x))
            .validate(x => if (x > 0)
              success
            else
              failure("number of threads should be greater than 0")
            )
            .text("number of threads to use"),

          opt[String]("globalClassFilter")
            .abbr("gcf")
            .action((x, c) => c.copy(globalClassFile = Some(x)))
            .validate(x => if ((new File(x)).isFile)
              success
            else
              failure(s"${x} is not a valid file")
            )
            .text("global class log file path"),

          opt[String]("datasetFilter")
            .optional()
            .valueName("<minIndex,maxIndex>")
            .abbr("df")
            .action((x, c) => c.copy(datasetFilter = x match {
              case regexDF(a, b) => a.toInt to b.toInt
              case _ => 0 to 0
            }))
            .validate {
              case regexDF(f1, f2) =>
                if (f2 < f1 || f1.toInt < 0 || f2.toInt < 0)
                  failure(s"[$f1,$f2] is an invalid range for dataset filter")
                else
                  success
              case f => failure(s"error parsing dataset range $f : format should be '(min,max)'")
            }
            .text("Limit the number of files processed (according to Regex filter on file names) default everything"),

          opt[String]("excludeResults")
            .optional()
            .valueName("<string,string,...>")
            .abbr("ex")
            .action((x, c) => c.copy(excludeString = Some(x.split(','))))
            .text("Limit the number of files processed according to filter on path default everything"),

          arg[File]("CampaignDirectory")
            .required()
            .valueName("<campaignDirectory>")
            .validate(checkDirectory)
            .action((x, c) => c.copy(campaignDir = x))
            .text(s"Directory containing all campaign files and results," +
              s" ex: data/leNet5"),
        ),

      note(sys.props("line.separator")),
      cmd("flows")
        .action((_, c) => c.copy(mode = FlowAnalysis))
      .children(
        arg[File]("cnnModelFile")
          .required()
          .valueName("<cnnModelJsonFile>")
          .validate(checkFile)
          .action((x,c) => c.copy(cnnModelFile = x))
          .text(s"Json Keras file containing the CNN model topology")
      ),
      cmd("coverage")
        .action((_,c) => c.copy(mode = Coverage))
        .children(
          opt[String]("injectionStrategy")
            .abbr("is")
            .action((x,c) => c.copy(injectionStrategy = x))
          .text("Name of the injection strategy to use (inputBase,inputOnly,preRegisters,postRegisters"),
          arg[File]("cnnModelFile")
            .required()
            .valueName("<cnnModelJsonFile>")
            .validate(checkFile)
            .action((x,c) => c.copy(cnnModelFile = x))
            .text(s"Json Keras file containing the CNN model topology")
        ),

      checkConfig(c => {
        if (c.mode == InjectionAnalysis) {
          if (!c.failureClassFile.isFile || !c.measureStrategyFile.isFile)
            failure("a failure class file and a measure strategy file must be provided")
          else
            success
        }
        else if (c.mode == FlowAnalysis || c.mode == EquivalenceClasses || c.mode == Coverage)
          success
        else
          failure("unknown command name")
      }
      )
    )
  }

def injectionAnalysis(config: Config): Unit = {

  for {
    campaignFiles <- FileManager.getCampaignFiles(config.campaignDir, MNIST, config.datasetFilter, config.excludeString)

    modelFile = campaignFiles.modelFile
    labelFile = campaignFiles.labelDictionary
    model <- CNNJsonParser.parseModel(modelFile)
    physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D)
    injections = campaignFiles.resultsFiles.keys.flatMap(f => Parser.parseCampaign(f, physicalModel)).toMap
    timeSpecs = retrieveTimeSpecs(physicalModel, injections)
  } yield {


    if (config.verbose) {
      println(s"Number of result files to parse: ${
        campaignFiles.resultsFiles.foldLeft(0) {
          _ + _._2.size
        }
      }")
    }


    /*reorder files by input index instead of origin directory, split the list into n equal sized sequences */
    val filesOrdered = campaignFiles
      .resultsFiles
      .foldLeft(Seq.empty[(File, File)]) { (l, r) =>
        r._2
          .map(
            f => (r._1, f)
          ) ++ l
      }
      .toList
      .sortBy(_._2.getName)
      .map(p => p.copy(_2 = Source.fromFile(p._2)))

    val sliding_size = if (filesOrdered.size >= config.threads) {
      filesOrdered.size / config.threads
    } else
      1
    val filesGrouped = filesOrdered
      .sliding(sliding_size, sliding_size)
      .toList

    /*generate the corresponding sequence of results iterator (for parallelization)*/
    val resultIterator = filesGrouped
      .map(p => Parser.parse(p, modelFile, labelFile))
    //val resultIterator = Parser.parse(filesOrdered, modelFile, labelFile).toStream
    val startTime = System.currentTimeMillis()

        // filter one injection by functional classes
        val resultStream = if (config.globalClassFile.isDefined) {
          val functionalClasses = parseFunctionalClassesLog(new File(config.globalClassFile.get))
          resultIterator.map(_.filter(c => functionalClasses.contains(c.injectionPoint.toString)))
        }
        else
          resultIterator

        val measureStrategy = parseMeasureStrategy(config.measureStrategyFile, config.failureClassFile) match {
          case Left(x) => throw x
          case Right(x) => x
        }
        val metrics = measureStrategy(resultStream)
        if (config.verbose) {
          metrics.toList.sortBy(_._1).foreach(p => println(s"${p._1}:\t${
            p._2
          }\n"))
        }
        val name = (for (n <- config.overrideMsName) yield n).getOrElse(measureStrategy.name)
        MeasureExporter.export(name, metrics.map(p => (p._1, Map("count" -> p._2.toDouble))))
    filesOrdered.foreach(_._2.close())

    println(s"total execution time=${(System.currentTimeMillis() - startTime).toInt} ms")
  }
}
  def flowAnalysis(config: Config) : Unit = {
    (for {
    cnnModel <- CNNJsonParser.parseModel(config.cnnModelFile)
    hwModel = cnnModel.sequential("dense", 10).sequential("dense_1", 21).appendLeft(cnnModel.input2D)
  } yield {
    val inputShape = cnnModel.input2D.inputShape
    val imageInput = (1 to inputShape.height*inputShape.width).toStream
    val flowModel =  hwModel.transform(imageInput)
   flowModel.layers.map(_.exportTFlows)
  }).getOrElse(println("error while parsing CNN model"))
  }

  def coverageAnalysis(config: Config) : Unit = {
    (for {
      cnnModel <- CNNJsonParser.parseModel(config.cnnModelFile)
      hwModel = cnnModel.sequential("dense", 10).sequential("dense_1", 21).appendLeft(cnnModel.input2D)
    } yield {
      val inputShape = cnnModel.input2D.inputShape
      val imageInput = (1 to inputShape.height*inputShape.width).toStream
     val injectionStrategy = config.injectionStrategy match {
        case "inputBased" => InjectionStrategy.Implicits.InputBased
        case "inputOnly" => InjectionStrategy.Implicits.InputOnly
        case "preRegister" => InjectionStrategy.Implicits.FirstRegisterBased
        case "postRegister" => InjectionStrategy.Implicits.OutputRegisterBased
        case _ => InjectionStrategy.Implicits.InputOnly
      }
     val coveredBf = hwModel.coveredBfCount(injectionStrategy,imageInput)
     val coveredSaX = hwModel.coveredSaXCount(injectionStrategy)
      println(s"Nb of SaX equivalent classes covered: ${coveredSaX}, Nb of BF equivalent classes covered: ${coveredBf}")
    }).getOrElse(println("error while parsing CNN model"))
  }

  def run(config: Config): Unit = {
    config.mode match {
      case InjectionAnalysis => injectionAnalysis(config)
      case FlowAnalysis => flowAnalysis(config)
      case Coverage => coverageAnalysis(config)
      case _ => println("unimplemented cli command")
    }
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      run(config)
    case _ =>
      print("Error config")
    // arguments are bad, error message will have been displayed
  }
}
