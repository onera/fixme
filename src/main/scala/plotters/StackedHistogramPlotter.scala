package plotters

import utils.FaultType.FaultType
import utils.FileManager._

import scala.sys.process._

object StackedHistogramPlotter extends Plotter {

  override def plot(diag: Gnuplot, dataFileName: String, faultType: FaultType): Unit = {
    //    if (System.getProperty("os.name").toLowerCase() == "linux") {
    for {dataFile <- getExistingMeasureFile(dataFileName)
         diagramFile <- getDiagramPlotFile(dataFileName)}
    yield {
      ("gnuplot -e \"set terminal " + diag.terminal + ";" +
        " set output '" + diagramFile.toString + "." + diag.terminal + "';" +
        " set style data histograms;" +
        " set style fill solid 0.5;" +
        " set key invert reverse Left outside;" +
        " set key autotitle columnheader;" +
        s" set yrange [0:${diag.ymax}];" +
        " set auto x;" +
        " set title '" + diag.title + "';" +
            " set xlabel '" + diag.xlabel + "';" +
            " set ylabel '" + diag.ylabel + "';" +
            " unset xtics;" +
            " set xtics nomirror rotate by -45 scale 0;" +
            " set style histogram rowstacked;" +
            " set style fill solid border -1;" +
            " set boxwidth 0.75;" +
            s" plot '${dataFile.getAbsolutePath}' using 2:xtic(1), for [i=3:" + diag.columnNumber + "] '' using i\"").!

        }
    //    }
    //    else
    //      println("Plotting figures use gnuplot on unix, it isn't available for " + System.getProperty("os.name").toLowerCase() + " os")
  }

  def plot(diag: Gnuplot, dataName: String): Unit = ???
}



