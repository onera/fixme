package operators

import models.Measures.toBoolean
import models._
import utils.UnionFind

import scala.io.BufferedSource

trait Parsable[T] {
  def parse(source: BufferedSource): T
}

object Parsable {

  trait Instances {

    implicit class ParsableOps(s: BufferedSource) {
      def parse[T](implicit e: Parsable[T]): T = e.parse(s)
    }

    implicit def EquivalenceClassesIsParsable[T](implicit equivalenceBuildable: EquivalenceBuildable[T]): Parsable[T] =
      (source: BufferedSource) =>
        equivalenceBuildable.buildFromClasses(source
          .getLines()
          .filterNot(_.isEmpty)
          .map(p => (p.split(',').toStream))
          .toStream)

    implicit object ScoreEqualityEquivalenceClassesMapIsParsable extends Parsable[ScoreEqualityEquivalenceClassesMap] {
      def parse(source: BufferedSource): ScoreEqualityEquivalenceClassesMap = {
        ScoreEqualityEquivalenceClassesMap(source.getLines()
          .filterNot(_.isEmpty)
          .map(p => p.split(','))
          .map(p => (p.head -> p.tail.toSeq))
          .toMap)
      }
    }

    implicit val globalMeasuresIsParsable: Parsable[Stream[GlobalMeasures]] = (source: BufferedSource) => {
      val content = source
        .getLines()
        .filterNot(_.isEmpty)
        .map(p => p.split(','))
      val header = content.next()
      val parsedStream = content.toStream.map(a => a.head.toInt -> header.tail.zip(a.tail.map(_.toInt)).toMap).sortBy(_._1)
      import GlobalMeasures._
      parsedStream.map(kv => {
        val p = kv._2
        GlobalMeasures(
          toBoolean(p("atLeastOneScoreModif")),
          toBoolean(p("alwaysPreserveClassif")),
          scoreDegradationList.map(e => toBoolean(p(s"alwaysTopScoreDegradedLeq${e}"))),
          scoreModifiedList.map(e => toBoolean(p(s"alwaysScoreModifiedLeq${e}"))),
          rankModifiedList.map(e => toBoolean(p(s"alwaysRankModifiedLeq${e}"))),
          softmaxDegradationList.map(e => toBoolean(p(s"alwaysSoftMaxDegradedLeq${e}"))),
          p("maxRankDowngrade")
        )
      }
      )
    }

    implicit val dataMeasuresIsParsable: Parsable[Stream[DataMeasures]] = (source: BufferedSource) => {
      val content = source
        .getLines()
        .filterNot(_.isEmpty)
        .map(p => p.split(','))
      val header = content.next()
      val parsedStream = content.toStream.map(a => header.zip(a).toMap)
      parsedStream.map(p => DataMeasures(
        p("injection"),
        p.keys.filter(_.contains("-misc")).toList.sorted.map(p(_).toInt),
        p("MisClassification").toInt,
        p("Observed").toInt,
        p("Masked").toInt
      ))
    }

    implicit val accuracyMeasuresIsParsable: Parsable[Stream[AccuracyMeasures]] = (source: BufferedSource) => {
      val content = source
        .getLines()
        .filterNot(_.isEmpty)
        .map(p => p.split(','))
      val header = content.next()
      val parsedStream = content.toStream.map(a => header.zip(a).toMap)
      parsedStream.map(p => AccuracyMeasures(
        p("injection"),
        p("countMissGold").toInt,
        p("countMissFaulty").toInt,
        p("countTotal").toInt
      ))
    }


    implicit val injectionMeasuresIsParsable: Parsable[Stream[InjectionMeasures]] = (source: BufferedSource) => {
      val content = source
        .getLines()
        .filterNot(_.isEmpty)
        .map(p => p.split(','))
      val header = content.next()
      val parsedStream = content.toStream.map(a => header.zip(a).toMap)
      parsedStream.map(p => InjectionMeasures(
        p("data"),
        p.keys.filter(_.contains("-misc")).toList.sorted.map(p(_).toInt),
        p("MisClassification").toInt,
        p("Observed").toInt,
        p("Masked").toInt
      ))
    }

    implicit object LocalEquivalenceClassesIsParsable extends Parsable[LocalEquivalenceClasses] {

      def parse(source: BufferedSource): LocalEquivalenceClasses =
        LocalEquivalenceClasses(source
          .getLines()
          .filterNot(_.isEmpty)
          .map(p => (p.split(',').toStream))
          .toStream)
    }

    implicit object UnionFindStringIsParsable extends Parsable[UnionFind[String]] {

      def parse(source: BufferedSource): UnionFind[String] = {
        val unionFind = UnionFind.empty[String]
        source
          .getLines()
          .filterNot(_.isEmpty)
          .foreach(p => {
            val equivalenceClass = p.split(',').toSeq
            if (equivalenceClass.size == 1)
              unionFind += equivalenceClass.head
            else {
              unionFind ++= equivalenceClass
              unionFind.addEquivalenceClass(equivalenceClass.toStream)
            }
          })
        unionFind
      }
    }
  }
}