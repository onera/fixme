package indicators

import models.{CampaignResult, ClassifiedResult}
import parsers.ProjectionScheme


case class MeasureStrategy(name: String, failureClasses: Seq[FailureClass], metrics: Seq[InjectionImpact], filter: (CampaignResult => Boolean), projection: ProjectionScheme) {

  def apply(campaignResults: Iterator[CampaignResult]): Map[String, Map[String, (Boolean, Boolean)]] = {
    campaignResults
      .filter(filter)
      .map(p => {
        val c = ClassifiedResult(projection(p), failureClasses.map(fc => (fc.name, fc.predicate(p))).filter(_._2).map(_._1).toList)
        c
      }
      ).toStream.groupBy(_.scenario).map(s => (s._1, metrics.map(m => (m.name, m(s._2))).toMap))
  }

  def apply(campaignResults: Seq[Iterator[CampaignResult]]): Map[String, Int] = {
    val c = campaignResults.par.map(p => {
      val t = this.apply(p)
      t
    }).reduce {
      (l, r) =>
        for {
          c <- l
        } yield {
          if (r.contains(c._1))
            (c._1, for {
              e <- c._2
            } yield {
              if (r(c._1).contains(e._1))
                (e._1, (r(c._1)(e._1)._1 && e._2._1, r(c._1)(e._1)._2 || e._2._2))
              else
                e
            })
          else
            c
        }

    }
    c.foldLeft(Map.empty[String, Int]) {
      (acc, local) =>
        if (acc.isEmpty)
          local._2.mapValues(b => if (b._1 && b._2) 1 else 0)
        else
          (for (k <- acc.keys) yield {
            (k, if (local._2(k)._1 && local._2(k)._2) acc(k) + 1 else acc(k))
          }).toMap
    }
  }

}

