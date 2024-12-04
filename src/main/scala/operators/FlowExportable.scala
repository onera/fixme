package operators

import operators.Transformable.{HierarchicalComponent, Flow}
import operators.all._
import utils.FileManager

import java.io.FileWriter
import scala.language.higherKinds

trait FlowExportable[-T[_]] {
  def exportFlowsDiagram[I: Streamable](x: T[I]): Unit

  def exportTFlows[I: Streamable](x: T[I]): Unit

}

object FlowExportable {

  private def exportTimeDiagram[I: Streamable](flows: Seq[Flow[I]], writer: FileWriter): Unit = {
    val dates = flows.map(_.tFlow.toDates)
    val data = flows.map(_.tFlow.toData)
    val lastDate = dates.flatten.max
    val tikz = for {i <- flows.indices
                    (f, d, n) = (dates(i), data(i), flows(i).id)
                    dataDuration = f.zip(f.tail).map(p => p._2 - p._1) :+ 1
                    } yield {
      s"""$$D_{$n}$$ & ${f.head - 1}U ${dataDuration.indices.map(i => s"${dataDuration(i)}D{d${d(i)}}").mkString(" ")} ${if (f.last != lastDate) s"${lastDate - f.last}U{} " else ""}\\\\
         |\t\t$$T_{$n}$$ & ${f.head - 1}L ${dataDuration.map(i => s"H ${if (i > 1) s"${i - 1}L" else ""}").mkString(" ")} ${if (f.last != lastDate) s"${lastDate - f.last}L" else ""}\\\\""".stripMargin
    }
    writer.write(
      s"""\\documentclass{standalone}
         |\\usepackage{tikz-timing}
         |\\usetikztiminglibrary{counters,columntype}
         |\\begin{document}
         |	\\begin{tikztimingtable}[node distance=0.5 pt,timing/xunit=1cm]
         |		\\emph{clock} & [C]${2 * lastDate} {c} \\\\
         |		\\emph{date} & [timing/counter/new={char=Q, max value=${lastDate}, wraps, text style={font=\\scriptsize}}] ${lastDate}{Q}\\\\
         |    ${tikz.mkString("\t", "\n\t\t", "")}
         |		\\extracode
         |		\\begin{background}[shift={(0.1,0)},help lines]
         |			\\vertlines{}
         |		\\end{background}
         |	\\end{tikztimingtable}
         |\\end{document}""".stripMargin)
    writer.close()
  }

  private def exportFlowDiag[I: Streamable](flows: Seq[Flow[I]], writer: FileWriter): Unit = {
    val dates = flows.map(_.tFlow.toDates)
    val data = flows.map(_.tFlow.toData)
    val lastDate = dates.flatten.max
    val tikz = for {i <- flows.indices
                    (f, d, n) = (dates(i), data(i), flows(i).id)
                    dataDuration = f.zip(f.tail).map(p => p._2 - p._1) :+ 1
                    } yield {
      s"""$${T_{$n}}$$ & ${f.head}L ${dataDuration.indices.map(i => s"H${if (dataDuration(i) > 1) s"${dataDuration(i) - 1}L" else ""}").mkString(" ")} ${if (f.last != lastDate) s"${lastDate - f.last}U{} " else ""}\\\\"""
    }
    writer.write(
      s"""\\documentclass{standalone}
         |\\usepackage{tikz-timing}
         |\\usetikztiminglibrary{counters,columntype}
         |\\begin{document}
         |	\\begin{tikztimingtable}[node distance=0.5 pt,timing/xunit=1cm]
         |		\\emph{clock} & [C]${2 * lastDate} {c} \\\\
         |		\\emph{date} & [timing/counter/new={char=Q, max value=${lastDate}, wraps, text style={font=\\scriptsize}}] ${lastDate}{Q}\\\\
         |    ${tikz.mkString("\t", "\n\t\t", "")}
         |		\\extracode
         |		\\begin{background}[shift={(0.1,0)},help lines]
         |			\\vertlines{}
         |		\\end{background}
         |	\\end{tikztimingtable}
         |\\end{document}""".stripMargin)
    writer.close()
  }

  private def exportTimeFlows[I: Streamable](flows: Seq[Flow[I]], writer: FileWriter): Unit = {
    writer.write("\\(\\begin{array}{ll}\n")
    flows.foreach(flow => {
      val split = flow.id.split('.')
      val regexp = """o([0-9]?)""".r
      val flowName = split.last match {
        case regexp(num) => s"f_{\\mathit{out}_{${num}}}"
        case "i" => "f_{\\mathit{in}}"
        case a: String => a
      }
      val id = s"""${split.dropRight(1).mkString(".")}.${flowName}"""
      writer.write(s"""T_{${id}} & = (${flow.tFlow.toDates.toArray.mkString(",")})\\\\\n""")
    })
    writer.write("\\end{array}\\)")
    writer.close()
  }

  trait Instances {

    implicit class ExportableOps[I:Streamable,T[_]](x:T[I]) {
      def exportFlowsDiagram(implicit ev: FlowExportable[T]): Unit = ev.exportFlowsDiagram(x)

      def exportTFlows(implicit ev: FlowExportable[T]): Unit = ev.exportTFlows(x)
    }

    implicit val equipmentIsExportable: FlowExportable[HierarchicalComponent] = new FlowExportable[HierarchicalComponent] {
      override def exportFlowsDiagram[I: Streamable](x: HierarchicalComponent[I]): Unit =
        for {writer <- FileManager.getFileWriter("flowsDiagrams", x.id + ".tex")} yield {
          //        exportTimeDiagram(x.input+: (x.registers.flatMap(_.outputs) ++ x.outputs.toSeq), writer)
          exportFlowDiag(x.input +: (x.registers.flatMap(_.outputs) ++ x.outputs.toSeq), writer)
        }

      def exportTFlows[I: Streamable](x: HierarchicalComponent[I]): Unit = {
        for {writer <- FileManager.getFileWriter("timeFlows", x.id + ".tex")} yield {
          exportTimeFlows(x.input +: (x.registers.flatMap(_.outputs) ++ x.outputs.toSeq), writer)
        }
      }
    }
  }
}
