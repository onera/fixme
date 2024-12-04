package indicators

import models.CampaignResult

object Predicate {
  type Predicate = CampaignResult => Boolean

  def identity: CampaignResult => Boolean = (p: CampaignResult) => true

  def &&(predicate1: Predicate, predicate2: Predicate): Predicate = (p: CampaignResult) => predicate1(p) && predicate2(p)

  def ||(predicate1: Predicate, predicate2: Predicate): Predicate = (p: CampaignResult) => predicate1(p) || predicate2(p)
}

