package plotters

import utils.FaultType.FaultType
import utils.FileManager.{createPlotFilePath, getDiagramPlotFile, getExistingMeasureFile}

import java.io.FileWriter
import scala.sys.process._

object MultiHistogramPlotter extends Plotter {

  override def plot(diag: Gnuplot, dataFileName: String, faultType: FaultType): Unit = {
    for {dataFile <- getExistingMeasureFile(dataFileName)
         diagramFile <- getDiagramPlotFile(dataFileName)}
    yield {
      ("gnuplot -e \"set terminal " + diag.terminal + ";" +
        " set output '" + diagramFile.getAbsolutePath + "." + diag.terminal + "';" +
        " set linetype 1 lc rgb \'blue\';" +
        " set style data histograms;" +
        " set style fill solid 0.5;" +
        " set key invert reverse Left outside;" +
        " set key autotitle columnheader;" +
        " set xtics nomirror rotate by -45 scale 0;" +
        " set title '" + diag.title + "';" +
        " set xlabel '" + diag.xlabel + "';" +
        " set ylabel '" + diag.ylabel + "';" +
        " plot '" + dataFile.getAbsolutePath + "' using 2:xtic(1), for [i=3:11] '' using i\"").!
    }
  }

  def plot(diag: Gnuplot, dataFileName: String): Unit = {
    for {dataFile <- getExistingMeasureFile(dataFileName)
         diagramFile <- createPlotFilePath(dataFileName)}
    yield {
      val cmd = ("set terminal " + diag.terminal + " size 1920,1080 font \"Helvetica,20\";" +
        " set output '" + diagramFile.toAbsolutePath.toString + "." + diag.terminal + "';" +
        //        " set linetype 1 lc rgb \'blue\';" +
        " set style data histograms;" +
        " unset autoscale;" +
        " set autoscale x;" +
        " set autoscale ymax;" +
        " set yrange [0:" + diag.ymax + "];" +
        " set style fill solid 1;" +
        " set grid ytics;" +
        s" set key right reverse Left outside title '${diag.key}' box;" +
        " set key autotitle columnheader;" +
        " set xtics;" +
        " set title '" + diag.title + "';" +
        " set xlabel '" + diag.xlabel + "';" +
        " set ylabel '" + diag.ylabel + "';" +
        " plot '" + dataFile.getAbsolutePath + s"' using 2:xtic(1), for [i=3:${diag.columnNumber+1}] '' using i")
      val gnuPlotFile = diagramFile.toAbsolutePath.toString.split('.').head + ".gnu"
      val fw = new FileWriter(gnuPlotFile) //TODO: move in fileManager
      fw.write(cmd.replace(";", ";\n"))
      fw.close()
      if (System.getProperty("os.name").contains("Linux")) {
        for {
          executable <- "which gnuplot".lineStream_!.headOption
        } yield {
          s"gnuplot -c ${gnuPlotFile}".!
        }
      }
    }
  }
}
