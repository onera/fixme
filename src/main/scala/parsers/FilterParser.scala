package parsers

import fastparse.NoWhitespace._
import fastparse._
import models.CampaignResult
import utils.FaultType.{BITFLIP, STUCK_AT_0, STUCK_AT_1}


object FilterParser {

  def filterParser[_: P]: P[CampaignResult => Boolean] = P(booleanExpression ~ End)

  def booleanExpression[_: P]: P[CampaignResult => Boolean] = P((("(" ~ booleanExpression ~ ")") | boolean) ~ (ws ~ logicOperator ~ ws ~ (("(" ~ booleanExpression ~ ")") | boolean)).?).map {
    case (lhs, Some(("||", rhs))) => (c: CampaignResult) => lhs(c) || rhs(c)
    case (lhs, Some(("&&", rhs))) => (c: CampaignResult) => lhs(c) && rhs(c)
    case (cond, None) => cond
  }

  def boolean[_: P]: P[CampaignResult => Boolean] = P(conditionInt | conditionString | conditionFaultType)

  def logicOperator[_: P]: P[String] = P("&&" | "||").!

  def conditionInt[_: P]: P[CampaignResult => Boolean] = P(int ~ ws ~ conditionOperator ~ ws ~ int).map {
    case (lhs, "==", rhs) => ((c: CampaignResult) => (lhs(c) == rhs(c)))
    case (lhs, "!=", rhs) => ((c: CampaignResult) => (lhs(c) != rhs(c)))
    case (lhs, ">=", rhs) => (c: CampaignResult) => (lhs(c) >= rhs(c))
    case (lhs, "<=", rhs) => (c: CampaignResult) => (lhs(c) <= rhs(c))
    case (lhs, ">", rhs) => (c: CampaignResult) => (lhs(c) > rhs(c))
    case (lhs, "<", rhs) => (c: CampaignResult) => (lhs(c) < rhs(c))
  }


  def conditionString[_: P]: P[CampaignResult => Boolean] = P(string ~ ws ~ testEquality ~ ws ~ string).map {
    case (lhs, "==", rhs) => ((c: CampaignResult) => (lhs(c).matches(rhs(c))))
    case (lhs, "!=", rhs) => (c: CampaignResult) => !(lhs(c).matches(rhs(c)))
  }

  def conditionFaultType[_: P]: P[CampaignResult => Boolean] = P(faultType ~ ws ~ testEquality ~ ws ~ faultType).map {
    case (lhs, "==", rhs) => (c: CampaignResult) => lhs(c).directoryName.matches(rhs(c).directoryName)
    case (lhs, "!=", rhs) => (c: CampaignResult) => !lhs(c).directoryName.matches(rhs(c).directoryName)
  }

  def conditionOperator[_: P]: P[String] = P(testEquality | ">=" | "<=" | "<" | ">").!

  def testEquality[_: P]: P[String] = P("==" | "!=").!


  def faultType[_: P] = P("STUCK_AT_0" | "STUCK_AT_1" | "BITFLIP" | "faultType").!.map {
    case "STUCK_AT_0" => (c: CampaignResult) => STUCK_AT_0
    case "STUCK_AT_1" => (c: CampaignResult) => STUCK_AT_1
    case "BITFLIP" => (c: CampaignResult) => BITFLIP
    case "faultType" => (c: CampaignResult) => c.injectionPoint.faultType
  }

  def int[_: P] = P(("bit" | "channel" | "dataId").! | integer).map {
    case "bit" => (c: CampaignResult) => c.injectionPoint.bitIndex
    case "channel" => (c: CampaignResult) => c.injectionPoint.channelIndex
    case "dataId" => (c: CampaignResult) => c.activation.index
    case i: Int => (c: CampaignResult) => i
  }

  def string[_: P]: P[CampaignResult => String] = P("layerId" | "dataLabel" | stringImm).!.map {
    case "layerId" => (c: CampaignResult) => c.injectionPoint.layerId.name
    case s: String => (c: CampaignResult) => s
    case "dataLabel" => (c: CampaignResult) => c.activation.label
  }

  def comment[_: P]: P[String] = P(("//" ~ AnyChar.rep) | ("/*" ~ AnyChar.rep ~ "*/")).!

  def ws[_: P]: P[Unit] = P(" ".rep.?)

  def stringImm[_: P]: P[String] = P("'" ~ CharIn("A-z,a-z,0-9").rep(1).! ~ "'").map(s => s)

  def integer[_: P]: P[Int] = P("-".? ~ digits).!.map(_.toInt)


  def digits[_: P]: P[Unit] = P(CharIn("0-9").rep(1))


}
