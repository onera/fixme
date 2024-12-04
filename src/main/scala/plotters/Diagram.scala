package plotters


trait Diagram {
  val title: String
  val xlabel: String
  val ylabel: String
}

case class Gnuplot(
                    title: String,
                    xlabel: String,
                    ylabel: String,
                    terminal: String = "png",
                    key: String,
                    ymax: String = "",
                    columnNumber: Int = 1
                  ) extends Diagram