package plotters

import utils.FaultType.FaultType

trait Plotter {
  def plot(diag: Gnuplot, dataName: String, faultType: FaultType): Unit

  def plot(diag: Gnuplot, dataName: String): Unit
}
