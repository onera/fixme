package parsers

import fastparse.NoWhitespace._
import fastparse._
import models.CampaignResult


object FailureClassParser {

  def failureClassParser[_: P]: P[CampaignResult => Boolean] = P(booleanExpression ~ ws ~ comment.?).map {
    case (l, _) => l
  }

  def booleanExpression[_: P]: P[CampaignResult => Boolean] = P((("(" ~ booleanExpression ~ ")") | boolean) ~ (ws ~ logicOperator ~ ws ~ (("(" ~ booleanExpression ~ ")") | boolean)).?).map {
    case (lhs, Some(("||", rhs))) => (c: CampaignResult) => lhs(c) || rhs(c)
    case (lhs, Some(("&&", rhs))) => (c: CampaignResult) => lhs(c) && rhs(c)
    case (cond, None) => cond
  }


  def boolean[_: P]: P[CampaignResult => Boolean] = P(conditionFloat | conditionString | forall | exists)

  def logicOperator[_: P]: P[String] = P("&&" | "||").!

  def conditionFloat[_: P]: P[CampaignResult => Boolean] = P((arithExpr | number) ~ ws ~ conditionOperator ~ ws ~ (arithExpr | number)).map {
    case (lhs, "==", rhs) => ((c: CampaignResult) => (lhs(c) == rhs(c)))
    case (lhs, "!=", rhs) => ((c: CampaignResult) => (lhs(c) != rhs(c)))
    case (lhs, ">=", rhs) => (c: CampaignResult) => (lhs(c) >= rhs(c))
    case (lhs, "<=", rhs) => (c: CampaignResult) => (lhs(c) <= rhs(c))
    case (lhs, ">", rhs) => (c: CampaignResult) => (lhs(c) > rhs(c))
    case (lhs, "<", rhs) => (c: CampaignResult) => (lhs(c) < rhs(c))
  }


  def conditionString[_: P]: P[CampaignResult => Boolean] = P(label ~ ws ~ testEquality ~ ws ~ label).map {
    case (lhs, "==", rhs) => ((c: CampaignResult) => (lhs(c).matches(rhs(c))))
    case (lhs, "!=", rhs) => (c: CampaignResult) => !(lhs(c).matches(rhs(c)))
  }

  def conditionOperator[_: P]: P[String] = P(testEquality | ">=" | "<=" | "<" | ">").!

  def testEquality[_: P]: P[String] = P("==" | "!=").!

  def arithExpr[_: P]: P[CampaignResult => Double] = P(addSub | abs)

  def abs[_: P] = P("|" ~ arithExpr ~ "|").map {
    case (absExp: (CampaignResult => Double)) => (c: CampaignResult) => math.abs(absExp(c))
  }

  def addSub[_: P]: P[CampaignResult => Double] = P(divMul ~ ws ~ (additionOperator ~ ws ~ divMul).rep).map {
    case (m, s) => (c: CampaignResult) =>
      s.foldLeft(m(c)) { (acc, p) =>
        p._1 match {
          case "+" => acc + p._2(c)
          case "-" => acc - p._2(c)
        }
      }
  }

  def divMul[_: P]: P[CampaignResult => Double] = P(factor ~ ws ~ (productOperator ~ ws ~ factor).rep).map {
    case (lhs, s) => (c: CampaignResult) =>
      s.foldLeft(lhs(c)) { (acc, d) =>
        d._1 match {
          case "*" => acc * d._2(c)
          case "/" => acc / d._2(c)
        }
      }
  }

  def factor[_: P] = P(("(" ~/ ws ~ arithExpr ~ ws ~ ")") | number)


  def additionOperator[_: P]: P[String] = P("+" | "-").!

  def productOperator[_: P]: P[String] = P("*" | "/").!

  def number[_: P]: P[CampaignResult => Double] = P(float | int)

  def float[_: P] = P(floatImm | attribute | max)

  def int[_: P] = P(integerImm | count)

  def labels[_: P]: P[String] = P("labels").!

  def label[_: P]: P[CampaignResult => String] = P(classification | stringImm | argmax)

  def attribute[_: P] = P(label ~ (score | rank | softmax) | (scores ~ "(" ~ digits.! ~ ")")).map {
    case ("expectedScores", d: String) => (c: CampaignResult) => c.goldScores.toList.sortBy(_._2).reverse(d.toInt)._2
    case ("observedScores", d: String) => (c: CampaignResult) => c.scores.toList.sortBy(_._2).reverse(d.toInt)._2
    case (l: (CampaignResult => String), f: (CampaignResult => String => Double)) => (c: CampaignResult) => f(c)(l(c))
  }

  def scoreType[_: P] = P("expectedScore" | "observedScore").!

  def scores[_: P] = P("expectedScores" | "observedScores").!

  def score[_: P]: P[CampaignResult => (String => Double)] = P(("." ~ scoreType ~ !".")).map {
    case ("expectedScore") => (c: CampaignResult) => c.goldScores
    case ("observedScore") => (c: CampaignResult) => c.scores
  }


  def rank[_: P]: P[CampaignResult => String => Double] = P("." ~ scoreType ~ ".rank").map {
    case ("expectedScore") => (c: CampaignResult) => (l: String) => c.goldScores.toList.sortBy(_._2).reverse.indexWhere(_._1.matches(l)).toDouble
    case ("observedScore") => (c: CampaignResult) => (l: String) => c.scores.toList.sortBy(_._2).reverse.indexWhere(_._1.matches(l)).toDouble
  }

  def softmax[_: P] = P("." ~ scoreType ~ ".softmax").map {
    case ("expectedScore") => (c: CampaignResult) => (l: String) => utils.Softmax.softmax(c.goldScores)(l)
    case ("observedScore") => (c: CampaignResult) => (l: String) => utils.Softmax.softmax(c.scores)(l)
  }

  def max[_: P] = P(scores ~ ".max").map {
    case ("expectedScores") => (c: CampaignResult) => c.goldScores.maxBy(_._2)._2
    case ("observedScores") => (c: CampaignResult) => c.scores.maxBy(_._2)._2
  }

  def argmax[_: P] = P(scores ~ ".argmax").map {
    case ("expectedScores") => (c: CampaignResult) => c.goldScores.maxBy(_._2)._1
    case ("observedScores") => (c: CampaignResult) => c.scores.maxBy(_._2)._1
  }


  def evalDouble(lhs: Double, op: String, rhs: Double) = op match {
    case ("==") => lhs == rhs
    case ("!=") => lhs != rhs
    case (">=") => lhs >= rhs
    case ("<=") => lhs <= rhs
    case (">") => lhs > rhs
    case ("<") => lhs < rhs
  }

  def internalArgument[_: P] = P(attribute | number).map {
    case (f) => (c: CampaignResult) => l: String => f(c)
  }

  def internalCondition[_: P] = P(((("label") ~ (score | rank | softmax)) | internalArgument) ~ ws ~ conditionOperator ~/ ws ~ ((("label") ~ (score | rank | softmax)) | internalArgument)).map {
    case (lhs: (CampaignResult => String => Double), op: String, rhs: (CampaignResult => String => Double)) => (c: CampaignResult) => (l: String) => evalDouble(lhs(c)(l), op, rhs(c)(l))
  }

  def forall[_: P]: P[CampaignResult => Boolean] = P("labels" ~ ".forall" ~/ "(" ~ ws ~ internalCondition ~ ws ~ ")").map {
    case (ic) => (c: CampaignResult) => c.goldScores.keys.forall(ic(c)(_))
  }

  def exists[_: P]: P[CampaignResult => Boolean] = P("labels" ~ ".exists" ~/ "(" ~ ws ~ internalCondition ~ ws ~ ")").map {
    case (ic) => (c: CampaignResult) => c.goldScores.keys.exists(ic(c)(_))
  }

  def count[_: P] = P("labels" ~ "." ~ "count" ~/ "(" ~ ws ~ internalCondition ~ ws ~ ")").map {
    case (ic) => (c: CampaignResult) => c.goldScores.keys.count(ic(c)(_)).toDouble
  }

  def classification[_: P]: P[CampaignResult => String] = P("observedClassif" | "expectedClassif").!.map {
    case "observedClassif" => (c: CampaignResult) => c.observedClassification.head
    case "expectedClassif" => (c: CampaignResult) => c.goldClassification.head
  }

  def comment[_: P]: P[String] = P(("//" ~ AnyChar.rep) | ("/*" ~ AnyChar.rep ~ "*/")).!

  def ws[_: P]: P[Unit] = P(" ".rep)

  def stringImm[_: P]: P[CampaignResult => String] = P("'" ~ CharIn("A-z,a-z,0-9").rep(1).! ~ "'").map {
    case (string) => (c: CampaignResult) => string
  }

  def integerImm[_: P]: P[CampaignResult => Double] = P("-".? ~ digits).!.map(p => (c: CampaignResult) => p.toDouble)

  def floatImm[_: P]: P[CampaignResult => Double] = P("-".? ~ digits ~ "." ~ digits).!.map(p => (_: CampaignResult) => p.toDouble)

  def digits[_: P]: P[Unit] = P(CharIn("0-9").rep(1))


}
