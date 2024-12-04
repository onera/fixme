package parsers

import indicators._
import io.circe._
import io.circe.parser.{parse => parseJson}
import models.LeNetFixture.LeNetFixtureSafecompPaper
import models._
import operators.all._
import parsers.FilterParser.filterParser
import parsers.InjectionImpactParser.injectionImpactParser

import java.io.File
import scala.io.Source

case class ProjectionScheme(args: Seq[String]) {
  private def toInt(c: String) = if (c.nonEmpty) c.toInt; else 0;

  def apply(campaignResult: CampaignResult): String = projection(campaignResult)

  val projection: (CampaignResult => String) = (campaignResult: CampaignResult) => {
    val default = List("layer", "channel", "time", "bit", "faultType", "data")
    val regExp = """(?:conv2d|maxpooling2d|dense)([0-9]*)""".r
    val instr = if (args.contains("injectionPoint") || args.contains("class"))
      default.filterNot(_.matches("data") && !args.contains("data"))
    else default.filter(args.contains(_))
    ((instr.foldLeft(" ") { (acc, arg) =>
      arg match {
        case "faultType" => acc + ":" + campaignResult.injectionPoint.faultType.directoryName
        case "layer" => acc + "" + (campaignResult.injectionPoint.layerId match {
          case l: Dense => (l.name.filterNot(_ == '_') match {
            case regExp(c) => if (c == "")
              s"FC1.i_"
            else s"FC${toInt(c) + 1}.i_"
            case _ => "unknown"
          })
          case l: Convolution2D => (l.name.filterNot(_ == '_') match {
            case regExp(c) => if (c == "") s"CONV1.i_" else s"CONV${toInt(c) + 1}.i_"
            case _ => "unknown"
          })
          case l: Pooling2D => (l.name.filterNot(_ == '_') match {
            case regExp(c) => if (c == "") s"POOL1.i_" else s"POOL${toInt(c) + 1}.i_"
            case _ => "unknown"
          })
          case _ => "unknown"
        })
        case "channel" => acc + "" + campaignResult.injectionPoint.channelIndex.toString
        case "bit" => acc + "+b" + campaignResult.injectionPoint.bitIndex.toString
        case "time" => acc + "@" + campaignResult.injectionPoint.startDate.toString
        case "data" => acc + ":onData_" + campaignResult.activation.index
        case _ => acc
      }
    }).filterNot(_.isWhitespace))
  }
}


object MeasureStrategyParser {

  implicit val unionFind = LeNetFixtureSafecompPaper.stuckAtXClasses

  implicit val decoderFailureClass: Decoder[FailureClass] = (hCursor: HCursor) => {
    val scPattern = """SC-([0-9]*)""".r
    val degradedPattern = """Degraded-(\d*.\d)""".r
    for {
      name <- hCursor.get[String]("name")
      string <- hCursor.get[String]("expression")
      parsedFailureClass = fastparse.parse(string, FailureClassParser.failureClassParser(_))
      failureClass <- if (parsedFailureClass.isSuccess)
        Right(OtherFailureClass(name, parsedFailureClass.get.value))
      else Left(DecodingFailure(s"error parsing failure class expression ${string}", Nil))
    } yield {
      failureClass
    }
  }

  implicit val decoderProjection: Decoder[ProjectionScheme] = (hCursor: HCursor) => {
    for {
      projectionString <- hCursor.as[Seq[String]]
    } yield {
      ProjectionScheme(projectionString)
    }
  }

  implicit val decoderInjectionImpact: Decoder[InjectionImpact] = (hCursor: HCursor) => for {
    name <- hCursor.get[String]("name")
    expressionStr <- hCursor.get[Seq[String]]("expression")
    expression <- {
      val parsed = expressionStr.map(s => fastparse.parse(s, injectionImpactParser(_)))
      if (parsed.forall(_.isSuccess))
        Right(parsed.map(_.get.value))
      else
        Left(DecodingFailure(s"error parsing injection impact: ${expressionStr}", Nil))
    }
  } yield {
    otherInjectionImpact(name, expression.head, expression.last)
  }


  def decoderFailureClasses(file: File) = {
    val s = Source.fromFile(file)
    val json = s.getLines().mkString("\n")
    s.close()
    for {
      doc <- parseJson(json)
      failureClasses <- doc.hcursor.get[Seq[FailureClass]]("failureClasses")
    } yield
      failureClasses
  }

  def decoderMetrics(file: File) = {
    val s = Source.fromFile(file)
    val json = s.getLines().mkString("\n")
    s.close()
    for {
      doc <- parseJson(json)
      name <- doc.hcursor.get[String]("name")
      metrics <- doc.hcursor.get[Seq[InjectionImpact]]("injectionImpacts")
      projection <- doc.hcursor.get[ProjectionScheme]("projection")
      filterStringOpt <- doc.hcursor.get[Option[String]]("filter")
      filter <- if (filterStringOpt.isDefined) {
        val filterString = filterStringOpt.get
        val parsed = fastparse.parse(filterString, filterParser(_))
        if (parsed.isSuccess)
          Right(parsed.get.value)
        else
          Left(DecodingFailure(s"error parsing filter: ${filterString}", Nil))
      }
      else
        Right(((c: CampaignResult) => true))
    } yield
      (name, metrics, projection, filter)
  }

  def parseMeasureStrategy(metricsFile: File, failureClassFile: File): Either[Error, MeasureStrategy] = {
    for {
      failureClasses <- decoderFailureClasses(failureClassFile)
      metrics <- decoderMetrics(metricsFile)
    } yield {
      MeasureStrategy(metrics._1, failureClasses, metrics._2, metrics._4, metrics._3)
    }

  }
}
