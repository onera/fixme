package utils


import utils.Dataset.Dataset

import java.io.{File, FileWriter}
import java.nio.file._
import java.text.SimpleDateFormat
import java.util.Calendar


case class CampaignFiles(modelFile: File, labelDictionary: File, resultsFiles: Map[File, Seq[File]], datasetLabelsFile: File)

object FileManager {
  private val OutputDirPath = Paths.get("output")
  private val OutputDirectory = createDirectory(OutputDirPath)
  private val DataDirPath = Paths.get("data")
  private val PlotDirectory = createDirectory(Paths.get(OutputDirPath.toString, "plot"))
  private val MeasuresDirectory = createDirectory(Paths.get(OutputDirPath.toString, "measures"))
  private val LogDirectory = createDirectory(Paths.get(OutputDirPath.toString, "logs"))
  private val TempDirectory = createDirectory(Paths.get(OutputDirPath.toString, "temp"))
  private val PlotDataDirectory = for {
    parent <- PlotDirectory
    directory <- createDirectory(Paths.get(parent.getAbsolutePath, "data"))
  } yield
    directory


  val figExtension = ".png"
  private val measuresExtension = ".dat"

  /**
   * If it does not exists, create a directory
   *
   * @param path the path where to create the directory
   * @return Some(File) or None if the creation failed
   */
  private def createDirectory(path: Path): Option[File] = {
    val directory = new File(path.toUri)
    if (directory.exists() || directory.mkdir()) {
      Some(directory)
    } else {
      println(s"Error while creating output directory ${path.toAbsolutePath}")
      None
    }
  }

  private def measureFileName(measureName: String) = measureName.replaceAll("""[/\\ ]""", "_")

  def createMeasureFilePath(measureName: String): Option[Path] =
    for {
      resultDirPath <- PlotDataDirectory
    } yield {
      Paths.get(resultDirPath.getAbsolutePath, measureFileName(measureName) + measuresExtension)
    }

  def getMeasureFileWriter(measureName: String): Option[FileWriter] =
    for {path <- createMeasureFilePath(measureName)}
      yield new FileWriter(new File(path.toAbsolutePath.toString))

  def getExistingMeasureFile(measureName: String): Option[File] =
    for {
      path <- createMeasureFilePath(measureName)
      file = new File(path.toAbsolutePath.toString) if file.exists() && file.isFile
    } yield
      file

  def createCsvFilePath(measureName: String, isTemporary: Boolean = false): Option[Path] =
    for {
      measureDirPath <- MeasuresDirectory
      resultDirPath <- if (!isTemporary) MeasuresDirectory else createDirectory(Paths.get(measureDirPath.toString, "tmp"))
    } yield {
      Paths.get(resultDirPath.getAbsolutePath, measureFileName(measureName) + ".csv")
    }

  def getCsvFileWriter(measureName: String, isTempFile: Boolean = false): Option[FileWriter] =
    for {path <- createCsvFilePath(measureName, isTempFile)}
      yield new FileWriter(new File(path.toAbsolutePath.toString))

  def getExistingCsvFile(measureName: String, isTemp: Boolean = false): Option[File] =
    for {
      path <- createCsvFilePath(measureName, isTemp)
      file = new File(path.toUri)
      if file.isFile
    }
    yield file

  def createPlotFilePath(dataName: String): Option[Path] =
    for {resultDirPath <- PlotDirectory} yield {
      Paths.get(resultDirPath.getAbsolutePath, measureFileName(dataName))
    }

  def getDiagramPlotFile(dataName: String): Option[File] =
    for {
      path <- createPlotFilePath(dataName)
    } yield {
      new File(path.toAbsolutePath.toString + figExtension)
    }

  def getFileWriter(folderName: String, fileName: String): Option[FileWriter] =
    for {
      output <- OutputDirectory
      folder <- createDirectory(Paths.get(output.getAbsolutePath, folderName))
    } yield {
      new FileWriter(new File(folder.getAbsolutePath + File.separator + fileName))
    }

  def getTempFilePath(name: String): Option[Path] = for {
    logDirPath <- TempDirectory
  } yield {
    Paths.get(logDirPath.getAbsolutePath, name)
  }

  def getTempFile(name: String): Option[File] = for {
    logFilePath <- getTempFilePath(name)
  } yield {
    new File(logFilePath.toString)
  }

  def locateTempFile(name: String): Option[File] =
    getTempFilePath(name) match {
      case Some(path) if Files.exists(path) => Some(new File(path.toAbsolutePath.toString))
      case _ => None
    }


  //Do not touch below these
  private def getListOfFilesInDirectory(directory: File): Option[List[File]] = {
    if (directory.exists && directory.isDirectory) {
      Some(directory.listFiles.filter(_.isFile).toList)
    } else {
      None
    }
  }

  private def getListOfDirectoriesInDirectory(directory: File): Option[List[File]] = {
    if (directory.exists && directory.isDirectory) {
      Some(directory.listFiles.filter(_.isDirectory).toList)
    } else {
      None
    }
  }

  private def getListOfResultsFiles(resultDirectory: File, filter: Range, exclude: Option[Seq[String]]) = {
    for {
      resultDirectories <- getListOfDirectoriesInDirectory(resultDirectory)
    } yield {
      for {
        resultDirectory <- exclude match {
          case Some(excludeString) => resultDirectories.filterNot(l => excludeString.exists(l.getName.contains))
          case None => resultDirectories
        }
        files <- getListOfFilesInDirectory(resultDirectory)
        injectionFile = Paths.get(resultDirectory.getAbsolutePath, "campaign.conf").toFile
        if injectionFile.isFile
      } yield {
        val regex = "datalog_(\\d*)".r
        (injectionFile,
          files
            .filter(file => file.getName match {
              case regex(v) => filter.contains(v.toInt)
              case _ => false
            })
        )
      }
    }.toMap
  }

  def clearTemps() = for {
    tempDir <- TempDirectory
    tempFiles <- getListOfFilesInDirectory(tempDir)
  } {
    tempFiles.foreach(f => Files.deleteIfExists(f.toPath))
  }

  def clearLogs() = for {
    logDir <- LogDirectory
    logFiles <- getListOfFilesInDirectory(logDir)
  } {
    logFiles.foreach(f => Files.deleteIfExists(f.toPath))
  }

  def getCampaignFiles(campaignDir: File, dataset: Dataset, filter: Range = 0 to Int.MaxValue, exclude: Option[Seq[String]] = None): Option[CampaignFiles] = {
    val campaignDirPath = campaignDir.toPath
    for {
      resultsFiles <- getListOfResultsFiles(Paths.get(campaignDirPath.toString, "results").toFile, filter, exclude)
      modelFile = Paths.get(campaignDirPath.toString, "architecture.json").toFile
      labelsFile = Paths.get(campaignDirPath.toString, "labelsDictionary.txt").toFile
      datasetLabelsFile = Paths.get(campaignDirPath.toString, dataset.fileName).toFile
      if modelFile.isFile && labelsFile.isFile
    } yield {
      CampaignFiles(modelFile, labelsFile, resultsFiles, datasetLabelsFile)
    }

  }

  def extractResourceAsFile(name: String): Option[String] = {
    val classLoader = getClass.getClassLoader
    for {resource <- Option(classLoader.getResource(name))}
      yield resource.getFile
  }

  private def getLogFile(name: String, ext: String = "") = {
    for {
      dir <- LogDirectory
    } yield
      new File(Paths.get(dir.getAbsolutePath, name + ext).toAbsolutePath.toString)
  }

  def saveTimeLog(operationName: String, time: Long): Unit = {
    for {
      file <- getLogFile("timeLogs", ".log")
    } {
      val fw = new FileWriter(file, true)
      val currentTime = Calendar.getInstance().getTime()
      val dateFormat = new SimpleDateFormat("<dd-MM:HH-mm-ss>")
      fw.append(s"${dateFormat.format(currentTime)} '${operationName}' ==> ${time} ms\n")
      fw.close()
    }

  }
}

