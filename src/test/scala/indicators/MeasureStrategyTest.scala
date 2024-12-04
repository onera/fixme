package indicators

import models.{CampaignResult, ClassifiedResult}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import parsers.Parser
import utils.Dataset.MNIST
import utils.FaultType.STUCK_AT_1
import utils.FileManager.extractResourceAsFile
import utils.{CampaignFiles, FileManager}

import java.io.File
import scala.io.{BufferedSource, Source}

class MeasureStrategyTest extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {

  private def getResultFiles(campaignFiles: CampaignFiles): Seq[(File, Seq[BufferedSource])] =
    campaignFiles.resultsFiles.toList.sortBy(_._1.getParentFile.getName).map(p => p.copy(_2 = p._2.map(Source.fromFile)))

  private def closeResultFiles(resultFiles: Seq[(File, Seq[BufferedSource])]): Unit =
    resultFiles.foreach(_._2.foreach(_.close()))

  "CountMeasure" should "compute the count of failure classes for each injection point in real campaign" in {
    for {
      resultsDir <- extractResourceAsFile("leNet5")
      campaignFiles <- FileManager.getCampaignFiles(new File(resultsDir), MNIST, exclude = Some(Seq("bitflip")))
      resultFiles = getResultFiles(campaignFiles)
      res = resultFiles.map(p => Parser.parse(p._2, p._1, campaignFiles.modelFile, campaignFiles.labelDictionary))
    } yield {
      def filter(c: CampaignResult) = c.injectionPoint.faultType == STUCK_AT_1

      def projection(result: CampaignResult): String = s""

      val metric = CountMetric("count", (s: Stream[ClassifiedResult]) => s.exists(_.classes.contains("SC-1")))
      val m = IntMeasure("count", List(XMisclassification(1)), filter, projection, metric)
      val metrics = res.par.map(p => m(p.toStream))
      closeResultFiles(resultFiles)
      println("ok")
    }
  }
  //    "MeasureStrategy" should "add correctly the resulting maps" in {
  //      implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
  //
  //      forAll((Gen.listOfN(8, Gen.listOfN(100, CampaignResultGen.dataGen)), "data"), minSuccessful(10)) {
  //        (data) =>
  //          val total = cMeasure("measure", List(OtherFailureClass("total", Predicate.identity)), Predicate.identity, (c: CampaignResult) => (c.injectionPoint.toString))
  //          val ms = MeasureStrategy("test", Seq(total))
  //          val d2 = data.map(_.toStream)
  //          val d3 = d2 ++ List(d2.head)
  //  //        val metrics = ms.measures.map(_(d3))
  //  //        metrics.head._2.forall(_._2("total") == d3.map(_.size).sum)
  //      }
  //    }
}
