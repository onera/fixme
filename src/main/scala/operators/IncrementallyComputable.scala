package operators

import models._
import parsers.Parser
import utils.FaultType.FaultType
import utils.FileManager.{createCsvFilePath, getTempFile, locateTempFile, saveTimeLog}

import java.io.File
import java.nio.file.{Files, StandardCopyOption}
import scala.annotation.tailrec
import scala.io.Source
import scala.util.hashing.MurmurHash3

trait IncrementallyComputable[T] {
  def reset(campaign: Campaign, operationName: String): Unit

  def compute(campaign: Campaign,
              operationName: String
             ): File

}

object IncrementallyComputable {
  val nbCores = Runtime.getRuntime.availableProcessors()

  trait Instance {
    implicit class CampaignOps(campaign: Campaign) {
      def computeScoreEquivalenceClasses(nameExtension: String = "")(implicit incrementallyComputable: IncrementallyComputable[ScoreEqualityEquivalenceClasses]): File =
        incrementallyComputable.compute(campaign, "scoreEquivalenceClasses" + nameExtension)

      def computeScoreEquivalenceClassesMap(nameExtension: String = "")(implicit incrementallyComputable: IncrementallyComputable[ScoreEqualityEquivalenceClassesMap]): File =
        incrementallyComputable.compute(campaign, "scoreEquivalenceClassesMap" + nameExtension)

      def computeGlobalMeasures(nameExtension: String = "")(implicit incrementallyComputable: IncrementallyComputable[Stream[GlobalMeasures]]): File =
        incrementallyComputable.compute(campaign, "globalMeasures" + nameExtension)

      def computeDataMeasures(nameExtension: String = "")(implicit incrementallyComputable: IncrementallyComputable[Stream[DataMeasures]]): File =
        incrementallyComputable.compute(campaign, "dataMeasures" + nameExtension)

      def computeInjectionMeasures(nameExtension: String = "")(implicit incrementallyComputable: IncrementallyComputable[Stream[InjectionMeasures]]): File =
        incrementallyComputable.compute(campaign, "injectionMeasures" + nameExtension)

      def computeAccuracyMeasures(nameExtension: String = "")(implicit incrementallyComputable: IncrementallyComputable[Stream[AccuracyMeasures]]): File =
        incrementallyComputable.compute(campaign, "accuracyMeasures" + nameExtension)

      def reset[T](operationName: String)(implicit incrementallyComputable: IncrementallyComputable[T]): Unit =
        incrementallyComputable.reset(campaign, operationName)
    }

    implicit def redExpBuildParsIsIncrementallyComputable[T](implicit
                                                             reducible: Reducible[T],
                                                             exportable: Exportable[T],
                                                             buildable: Buildable[T],
                                                             parseableT: Parsable[T]
                                                            ): IncrementallyComputable[T] = new IncrementallyComputable[T] {

      def getFileName(inputFiles: List[File], operationName: String): String = MurmurHash3.stringHash(operationName + inputFiles.map(_.getAbsolutePath)).toString

      def compute(campaign: Campaign,
                  operationName: String): File = {
        val startDate = System.currentTimeMillis()
        val inputFiles = campaign.results.keys.flatMap(k => campaign.results(k).map(v => v -> k)).toMap
        val size = inputFiles.size
        val finalResultLogName = getFileName(inputFiles.keys.toList, operationName)
        val resFile = locateTempFile(finalResultLogName) match {
          case Some(value) => {
            println(s"Found existing checkpoint of operation ${operationName}")
            value
          }
          case None =>
            val initialResults = slicingFiles(inputFiles.keys.toList).par.map(files => {

              val resultFile = getTempFile(getFileName(files, operationName)).get
              initialComputeBlock(files.map(p => p -> inputFiles(p)).toMap, campaign, resultFile, size)
            }).toList
            val finalTemp = loop(initialResults, operationName, size).head
            val finalLog = getTempFile(finalResultLogName).get
            Files.copy(finalTemp.toPath, finalLog.toPath, StandardCopyOption.REPLACE_EXISTING)
            val durationTime = System.currentTimeMillis() - startDate
            println(s"Computation took: ${durationTime} ms")
            saveTimeLog(operationName, durationTime)
            finalLog
        }
        println(resFile.toPath.toString)
        Files.copy(resFile.toPath, createCsvFilePath(operationName).get.toAbsolutePath, StandardCopyOption.REPLACE_EXISTING)
        println(s"Results wrote in ${createCsvFilePath(operationName).get.toAbsolutePath.toString}")
        resFile
      }

      @tailrec
      def loop(tempFiles: List[File], operationName: String, size: Int): List[File] =
        if (tempFiles.size == 1)
          tempFiles
        else
          loop(
            for {
              files <- slicingFiles(tempFiles)
              resultFile <- getTempFile(getFileName(files, operationName))
            } yield
              intermediateComputeBlock(files, resultFile, size), operationName, size)


      def slicingFiles(files: List[File]): List[List[File]] = {
        files.sortBy(_.getName).sliding(nbCores, nbCores).toList
      }

      var nbBuildOver = 0

      def updateInitialBuild(size: Int) = {
        synchronized {
          nbBuildOver += 1
          if (nbBuildOver % (100) == 0 || (size < 100 && nbBuildOver > (size - 100))) println(s"initial building: ${nbBuildOver * 100 / size} %")
        }
      }

      var nbInitialMerged = 0

      def updateInitialMerged(size: Int) = {
        synchronized {
          val sizeMerge = (nbCores - 1) * (size / nbCores) + (size % nbCores - 1)
          nbInitialMerged += 1
          if (nbInitialMerged % (100) == 0 || (sizeMerge < 100 && nbInitialMerged > (sizeMerge - 100))) println(s"initial merging: ${nbInitialMerged * 100 / sizeMerge} %")
        }
      }

      var nbIntermediateMerged = 0

      def updateIntermediateMerged(size: Int) = {
        synchronized {
          nbIntermediateMerged += 1
          val mSize = size / nbCores
          if (nbIntermediateMerged % (100) == 0 || (mSize < 100 && nbIntermediateMerged > (mSize - 100))) println(s"incremental merging: ${nbIntermediateMerged * 100 / mSize} %")
        }
      }

      def initialComputeBlock(files: Map[File, FaultType], campaign: Campaign, resultFile: File, size: Int): File = (for {
        file <- locateTempFile(resultFile.getName)
      } yield {
        println(s"Found existing checkpoint")
        updateInitialBuild(size)
        synchronized {
          nbInitialMerged += files.size - 1
        }
        file
      }).getOrElse {
        val sources = files.par.map(kv => kv._1 -> Source.fromFile(kv._1))
        exportable.export(
          resultFile.getName,
          sources
            .map(kv => Parser.parse(kv._2, campaign.strategy(files(kv._1)), campaign.labels).toStream)
            .map(p => {
              val res = buildable.buildFrom(p)
              updateInitialBuild(size)
              res
            }).toStream
            .reduce((l, r) => {
              val res = reducible.reduce(l, r)
              updateInitialMerged(size)
              res
            }))
        sources.foreach(_._2.close())
        resultFile
      }

      def intermediateComputeBlock(files: List[File], resultFile: File, size: Int): File = (for {
        file <- locateTempFile(resultFile.getName)}
      yield {
        println("Found existing checkpoint")
        updateIntermediateMerged(size)
        file
      }).getOrElse {
        val sources = files.par.map(Source.fromFile)
        exportable.export(
          resultFile.getName,
          sources
            .map(parseableT.parse)
            .reduce((l, r) => {
              val red = reducible.reduce(l, r)
              updateIntermediateMerged(size)
              red
            })
        )
        sources.foreach(_.close())
        resultFile
      }

      def loopReset(files: List[File], operationName: String): List[File] = {
        if (files.size == 1)
          files
        else
          loopReset(for {
            tempFiles <- slicingFiles(files)
            resultFile <- getTempFile(getFileName(tempFiles, operationName))
          } yield {
            Files.deleteIfExists(resultFile.toPath)
            resultFile
          }, operationName)
      }

      def reset(campaign: Campaign, operationName: String): Unit = {
        val inputFiles = campaign.results.keys.flatMap(k => campaign.results(k).map(v => v -> k)).toMap
        val finalResultLogName = getFileName(inputFiles.keys.toList, operationName)
        val resFile = getTempFile(finalResultLogName).get
        Files.deleteIfExists(resFile.toPath)
        val initialResults = slicingFiles(inputFiles.keys.toList).par.map(files => {
          val resultFile = getTempFile(getFileName(files, operationName)).get
          Files.deleteIfExists(resultFile.toPath)
          resultFile
        }).toList
        Files.deleteIfExists(loopReset(initialResults, operationName).head.toPath)
      }

    }
  }
}
