package models

import org.apache.spark.sql.SparkSession
import parsers.Parser
import parsers.Parser.{InjectionStrategyDictionary, ResultsFileMap}
import utils.{Image, FaultType}
import java.io.File
import scala.io.Source

case class Campaign(model: ConvolutionalNeuralNetwork, labels: Map[Int, String], dataSet: List[Image], strategy: InjectionStrategyDictionary, results: ResultsFileMap) {


  def apply(injectionPoint: Injection): Seq[Option[CampaignResult]] = (for {
    data <- dataSet
    dataFile <- results(injectionPoint.faultType).find(p => p.getName.matches(s"datalog_${data.index}"))
  } yield {
    val s = Source.fromFile(dataFile)
    val value = Parser.parseInjection(s, strategy(injectionPoint.faultType), labels, injectionPoint)
    s.close()
    value
  })

  def apply(data: Image): Option[Seq[CampaignResult]] = {
    strategy.keys.map({ faultType =>
      for {
        dataFile <- results(faultType).find(p => p.getName.matches(s"datalog_${data.index}"))
      } yield {
        val s = Source.fromFile(dataFile)
        val value = Parser.parse(s, strategy(faultType), labels).foldLeft(Seq.empty[CampaignResult]) { (acc, r) => acc :+ r }
        s.close()
        value
      }
    }).reduce { (l, r) => if (l.isDefined && r.isDefined) Some(l.get ++ r.get) else None }
  }

}
