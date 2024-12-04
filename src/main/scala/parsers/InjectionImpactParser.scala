package parsers

import fastparse.NoWhitespace._
import fastparse._
import models.ClassifiedResult

object InjectionImpactParser {
  def injectionImpactParser[_: P]: P[Stream[ClassifiedResult] => Boolean] = P(booleanExpression ~ End)

  def booleanExpression[_: P]: P[Stream[ClassifiedResult] => Boolean] = P(boolean ~ (ws ~ logicOperator ~ ws ~ boolean).rep(1).?).map {
    case (lhs, Some(seq)) => seq.foldLeft(lhs) {
      (lhs, p) =>
        p match {
          case ("||", rhs: (Stream[ClassifiedResult] => Boolean)) => (c: Stream[ClassifiedResult]) => lhs(c) || rhs(c)
          case ("&&", rhs: (Stream[ClassifiedResult] => Boolean)) => (c: Stream[ClassifiedResult]) => lhs(c) && rhs(c)
        }
    }
    case (cond, None) => cond
  }

  def not[_: P] = P("!").!

  def boolean[_: P]: P[Stream[ClassifiedResult] => Boolean] = P(not.? ~ ((exists | forall) | ("(" ~ booleanExpression ~ ")") | trueFalse)).map {
    case (Some(_), b: (Stream[ClassifiedResult] => Boolean)) => (c: Stream[ClassifiedResult]) => !b(c)
    case (None, b: (Stream[ClassifiedResult] => Boolean)) => b
  }

  def trueFalse[_: P]: P[Stream[ClassifiedResult] => Boolean] = P("true" | "false").!.map {
    case "true" => (c: Stream[ClassifiedResult]) => true
    case "false" => (c: Stream[ClassifiedResult]) => false
  }

  def exists[_: P]: P[Stream[ClassifiedResult] => Boolean] = P("exists" ~ "(" ~ argument ~ ")").map {
    case (s: String) => (c: Stream[ClassifiedResult]) => c.exists(p => p.classes.contains(s))
  }

  def forall[_: P]: P[Stream[ClassifiedResult] => Boolean] = P("forall" ~ "(" ~ argument ~ ")").map {
    case (s: String) => (c: Stream[ClassifiedResult]) => c.forall(p => p.classes.contains(s))
  }

  def argument[_: P] = P(stringImm)

  def logicOperator[_: P]: P[String] = P("&&" | "||").!

  def comment[_: P]: P[String] = P(("//" ~ AnyChar.rep) | ("/*" ~ AnyChar.rep ~ "*/")).!

  def ws[_: P]: P[Unit] = P(" ".rep.?)

  def stringImm[_: P]: P[String] = P("'" ~ CharsWhile(c => c != ''').! ~ "'").map(s => s)

  def integer[_: P]: P[Int] = P("-".? ~ digits).!.map(_.toInt)


  def digits[_: P]: P[Unit] = P(CharIn("0-9").rep(1))


}