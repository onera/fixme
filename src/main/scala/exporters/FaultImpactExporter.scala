package exporters

import exporters.Exporter.CampaignResults
import indicators._
import models.{ConvolutionalNeuralNetwork, Injection}
import parsers.MeasureResultsParser.parseFaultImpactCheckPoints
import parsers.Parser.analyzeResults
import utils.FileManager.getExistingCsvFile
import utils.{FaultType, FileManager}

import java.io.File

object FaultImpactExporter extends Exporter with DataFileWriter {


  def export(name: String, data: Seq[CampaignResults], enableWrite: Boolean = false, enableTemp: Boolean = false) = {
    val analyzedResultsp = data.par.map(d => analyzeResults(d).foldLeft(Map.empty[Injection, Map[FailureClass, Int]]) { (z, v) =>
      (for {
        iMapelement <- z.find(_._1 == v.injectionPoint)
      } yield {
        (for {
          fcElement <- iMapelement._2.find(_._1 == v.failureClass)
        } yield {
          z ++ Map(v.injectionPoint -> (iMapelement._2 ++ Map(v.failureClass -> (fcElement._2 + 1))))
        }).getOrElse(z ++ Map(v.injectionPoint -> (iMapelement._2 ++ Map(v.failureClass -> 1))))
      }).getOrElse(z ++ Map(v.injectionPoint -> Map(v.failureClass -> 1)))
    })
    val analyzedResults = analyzedResultsp
      .reduce((z, v) => z ++ v.map { case (k, v) => if (!z.keys.exists(_ == k)) k -> v else k -> (z(k) ++ v.map { case (kk, vv) => if (!z(k).keys.exists(_ == kk)) kk -> vv else kk -> (z(k)(kk) + vv) }) })

    if (enableWrite) writeFile(analyzedResults, name, enableTemp)
    analyzedResults
  }

  def writeFile(analyzedResults: Map[Injection, Map[FailureClass, Int]], name: String, isTemp: Boolean = false) = for {
    injectionsFileWriter <- FileManager.getCsvFileWriter(name, isTemp)
  } yield {
    val injectionsStr = analyzedResults.zipWithIndex.toList.sortBy(_._2).map(inj => s"${inj._2}," +
      s"${inj._1._1.layerId.name}," +
      s"${inj._1._1.channelIndex}," +
      s"${inj._1._1.bitIndex}," +
      s"${inj._1._1.faultType.toString}," +
      s"${inj._1._1.startDate}," +
      s"${inj._1._1.stopDate}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(1) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(2) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(3) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(4) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(5) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(6) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(7) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(8) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case XMisclassification(9) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case Degraded(_) => true; case _ => false }).values.sum}," +
      s"${inj._1._2.filter(_._1 match { case Masked => true; case _ => false }).values.sum}").mkString("\n")
    injectionsFileWriter.write("id,layer,channel,bit,faultType,start,stop,SC-1,SC-2,SC-3,SC-4,SC-5,SC-6,SC-7,SC-8,SC-9,Observed,Masked\n" + injectionsStr)
    injectionsFileWriter.close()
  }

  def restoreCheckpoint(model: ConvolutionalNeuralNetwork, isTemp: Boolean, names: String*): Option[Map[Injection, Map[FailureClass, Int]]] = {
    for {
      files <- names.map(i => getExistingCsvFile(i, isTemp))
        .foldLeft[Option[Seq[File]]](Some(Seq.empty[File]))((a, b) => if (a.isEmpty || b.isEmpty) None; else Some(a.get :+ b.get))
    } yield {
      val dataframe = parseFaultImpactCheckPoints(files: _*)
      val failureClasses = (1 to 9).map(i => XMisclassification(i)) :+ Degraded(0.0.toFloat) :+ Masked
      val res = dataframe.collect().map(row =>
        for {
          layer <- model.layers.find(_.name == row.getString(1))
        } yield {
          (Injection(layer,
            row.getInt(2),
            row.getInt(3),
            FaultType.withName(row.getString(4)),
            row.getInt(5),
            row.getInt(6))
            -> (7 until 18).map(i => row.getInt(i)))
        }
      )
      res
        .filter(_.isDefined)
        .map(_.get)
        .groupBy(_._1)
        .map(r => (r._1, r._2
          .map(_._2)
          .reduce(_.zip(_)
            .map(e => e._1 + e._2))
          .zip(failureClasses)
          .map(p => p._2 -> p._1)
          .toMap
        ))
    }
  }


  def export(name: String, data: Map[String, Map[String, Double]], enablePlot: Boolean): Unit = {

  }

  def `export`(name: String, data: CampaignResults, enablePlot: Boolean): Unit = ???
}
