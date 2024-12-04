package exporters

import utils.FileManager.{createCsvFilePath, getCsvFileWriter, getTempFile, getMeasureFileWriter}

import java.io.{BufferedWriter, File, FileWriter}

trait DataFileWriter {

  def writeDataFile[Tk, T](dataName: String, data: Seq[(Tk, T)], header: String = ""): Unit = {
    for {dataFile <- getMeasureFileWriter(dataName)}
      yield {
        val bw = new BufferedWriter(dataFile)
        if (header.nonEmpty) bw.write(header + "\n")
        for ((x, y) <- data) {
          y match {
            case l: Iterable[_] => bw.write(s"$x ${l.mkString(" ")}\n")
            case _ => bw.write(s"$x $y\n")
          }
        }
        bw.close()
      }
  }

  def writeDataFile(dataName: String, data: Map[String, Map[String, Double]], header: List[String]): Unit = {
    for {dataFile <- getMeasureFileWriter(dataName)}
      yield {
        val bw = new BufferedWriter(dataFile)
        if (header.nonEmpty) bw.write(header.map(_.replace(" ", "-")).mkString(" ") + "\n")
        for ((x, y) <- data) {
          bw.write(x.replace(" ", "_") + " ")
          header.tail.foreach(s => {
            if (y.contains(s))
              bw.write(y(s).toString)
            else
              bw.write("0.0")
            bw.write(" ")
          })
          bw.write("\n")
        }
        bw.close()
      }
  }

  def writeCsvFile[V](name: String, metric: Map[String, Map[String, V]], columnHeaders: List[String], comment: String = ""): Option[File] = {
    for {
      fw <- getCsvFileWriter(name)
      file <- createCsvFilePath(name)
    } yield {
      if (comment.nonEmpty)
        fw.write(comment + "\n")
      fw.write(columnHeaders.mkString(", ") + "\n")
      metric.foreach(m => {
        fw.write(m._1 + ", ")
        columnHeaders.tail.foreach(h => fw.write(m._2(h).toString + ", "))
        fw.write("\n")
      })
      fw.close()
      new File(file.toString)
    }
  }

  def writeLogFile[T: Ordering, U](name: String, metric: Map[T, Map[String, U]], columnHeaders: List[String], comment: String = ""): Option[File] = {
    for {
      f <- getTempFile(name)
      fw = new FileWriter(f)
    } yield {
      if (comment.nonEmpty)
        fw.write(comment + "\n")
      fw.write(columnHeaders.mkString(",") + "\n")
      metric.toSeq.sortBy(_._1).foreach(m => {
        fw.write(m._1 + ",")
        fw.write(columnHeaders.tail.map(h => m._2(h).toString).mkString(","))
        fw.write("\n")
      })
      fw.close()
      f
    }
  }

  def writeLogClassesFile(name: String, metric: Iterable[Iterable[String]], comment: String = ""): Option[File] = {
    for {
      f <- getTempFile(name)
      fw = new FileWriter(f)
    } yield {
      if (comment.nonEmpty)
        fw.write(comment + "\n")
      metric.foreach(m => {
        fw.write(m.mkString(","))
        fw.write("\n")
      })
      fw.close()
      f
    }
  }

  def writeLogClassesMapFile(name: String, classes: Map[String, Seq[String]]): Option[File] = {
    for {
      f <- getTempFile(name)
      fw = new FileWriter(f)
    } yield {
      classes.foreach(m => {
        fw.write(s"${m._1},${m._2.mkString(",")}\n")
      })
      fw.close()
      f
    }
  }
}

object FileWriter extends DataFileWriter
