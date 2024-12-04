package models

trait EquivalenceClasses {
  val classes: Stream[Stream[String]]
}

case class ScoreEqualityEquivalenceClasses(classes: Stream[Stream[String]]) extends EquivalenceClasses

case class OrderEquivalenceClasses(classes: Stream[Stream[String]]) extends EquivalenceClasses

case class LocalEquivalenceClasses(classes: Stream[Stream[String]]) extends EquivalenceClasses

trait EquivalenceClassesMap {
  val classes: Map[String, Seq[String]]
}

case class ScoreEqualityEquivalenceClassesMap(classes: Map[String, Seq[String]]) extends EquivalenceClassesMap

case class OrderEquivalenceClassesMap(classes: Map[String, Seq[String]]) extends EquivalenceClassesMap
