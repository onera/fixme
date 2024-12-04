package plotters

import utils.FaultType.FaultType
import utils.FileManager._

import scala.sys.process._

object HistogramPlotter extends Plotter {


  override def plot(diag: Gnuplot, dataFileName: String, faultType: FaultType): Unit = {
    for {dataFile <- getExistingMeasureFile(dataFileName)
         diagramFile <- getDiagramPlotFile(dataFileName)
         }
    yield {
      ("gnuplot -e \"set terminal " + diag.terminal + " size 1920,1080 font \"Helvetica,20\";" +
        " set linetype 1 lc rgb \'dark-orange\';" +
        " unset autoscale;" +
        " set autoscale x;" +
        " set autoscale ymax;" +
        " set yrange [0:" + diag.ymax + "];" +
        " set output '" + diagramFile.getAbsolutePath + "." + diag.terminal + "';" +
        " set style data histograms;" +
        " set style fill solid 0.5;" +
        " set xtics nomirror rotate by -45 scale 0;" +
        " set key under nobox;" +
        " set title '" + diag.title + "';" +
        " set xlabel '" + diag.xlabel + "';" +
        " set ylabel '" + diag.ylabel + "';" +
        " plot '" + dataFile.getAbsolutePath + "' using 2:xtic(1) title '" + diag.key + "'\"").!
    }
  }

  def plot(diag: Gnuplot, dataFileName: String): Unit = {
    for {dataFile <- getExistingMeasureFile(dataFileName)
         diagramFile <- createPlotFilePath(dataFileName)
         }
    yield {
      ("gnuplot -e \"set terminal " + diag.terminal + ";" +
        " set linetype 1 lc rgb \'dark-orange\';" +
        " unset autoscale;" +
        " set autoscale x;" +
        " set autoscale ymax;" +
        " set yrange [0:" + diag.ymax + "];" +
        " set output '" + diagramFile.toAbsolutePath.toString + "';" +
        " set style data histograms;" +
        " set style fill solid 0.5;" +
        " set xtics nomirror rotate by -45 scale 0;" +
        " set key under nobox;" +
        " set title '" + diag.title + "';" +
        " set xlabel '" + diag.xlabel + "';" +
        " set ylabel '" + diag.ylabel + "';" +
        " plot '" + dataFile.getAbsolutePath + "' using 2:xtic(1) title '" + diag.key + "'\""
        ) //.!
    }

  }
}
