package exporters

import exporters.Exporter.CampaignResults
import models.CampaignResult

trait Exporter {
  def export(name: String, data: CampaignResults, enablePlot: Boolean): Unit
}

object Exporter {
  type CampaignResults = Stream[CampaignResult]

}