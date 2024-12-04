package exporters

import models.CampaignResult
import plotters.{Gnuplot, MultiHistogramPlotter}


object MeasureExporter extends DataFileWriter with Exporter {
  def export(name: String, metrics: Map[String, Map[String, Double]], enablePlot: Boolean = false): Unit = {
    val columnHeaders = List("scenarios") ++ metrics.foldLeft(Map.empty[String, Double]) { (acc, value) => acc ++ value._2 }.keys.toList.sortBy(_.toString)
    writeCsvFile(name, metrics, columnHeaders)
    if (enablePlot) {
      writeDataFile(name, metrics, columnHeaders)

      val diagram = Gnuplot(
        title = name,
        xlabel = "scenario",
        ylabel = "failureClass",
        terminal = "svg",
        key = name,
        ymax = "100",
        columnNumber = columnHeaders.size - 1
      )
      MultiHistogramPlotter.plot(diagram, name)
    }
  }

  def export(name: String, data: Stream[CampaignResult], enablePlot: Boolean): Unit = ???
}
