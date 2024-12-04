package indicators

import models.ClassifiedResult
import utils.UnionFind

trait Metric[V] {
  val name: String

  def apply(data: Stream[ClassifiedResult]): V
  //  def apply(data: Seq[Stream[ClassifiedResult]]): V
}

object Metric

case class OtherMetric[V](name: String, expression: Stream[ClassifiedResult] => V) extends Metric[V] {
  def apply(data: Stream[ClassifiedResult]): V = expression(data)
}

case class CountMetric(name: String, impactClass: Stream[ClassifiedResult] => Boolean) extends Metric[Int] {
  def apply(res: Stream[ClassifiedResult]): Int =
    res.groupBy(_.scenario).count(p => impactClass(p._2))

  //  def apply(res: Seq[Stream[ClassifiedResult]]): Int =
  //    res.par.map(_.groupBy(_.scenario).count(p => impactClass(p._2))

}

case class ClassesBySizeMetric(name: String, impactClass: Stream[ClassifiedResult] => Boolean)(implicit val unionFind: UnionFind[String]) extends Metric[Map[String, Int]] {
  def apply(res: Stream[ClassifiedResult]): Map[String, Int] = {
    val equivalentClasses = unionFind.equivalenceClasses
    res.groupBy(_.scenario).filter(p => impactClass(p._2)).keys.map(k => (k,
      (for {
        c <- equivalentClasses.find(_.contains(k.split('@').head.split('+').head))
      }
      yield
        c.size).getOrElse(0)
    )).toMap
  }
}

//
//case class CountClassesMetric(name: String, impactClass: ClassifiedResult => Boolean)(implicit val config: (UnionFind[String], Seq[Layer2D])) extends Metric[Set[(Set[String], Int)]] {
//  override val expression: (Stream[ClassifiedResult] => Set[(Set[String], Int)]) = (res: Map[String, Map[String, Double]]) => {
//    val layers = config._2
//    val unionFind = config._1
//    val impactRes = res
//      .mapValues(impactClass)
//    val impactFiltered = impactRes.filter(_._2).foldLeft(Map.empty[String, Int]) {
//      (acc, p) => {
//        val d = p._1.split(":").head.split('+')
//        val key = if (p._1.contains("BITFLIP")) d.head else d.head.split('@').head
//        if (!acc.contains(key))
//          (acc ++ Map(key -> 1))
//        else
//          (acc ++ Map(key -> (acc(key) + 1)))
//      }
//    }
//
//    val classes = impactFiltered.map(p => (unionFind.equivalenceClasses.find(s => s.contains(p._1)), p._2))
//    classes
//      .filter(l =>
//        l._1.isDefined
//      )
//      .map(p => (p._1.get.filterNot(id => layers.exists(l => id.contains(s"${l.name}.i") || id.contains(s"${l.name}.o"))), p._2))
//      .toSet
//
//
//  }
//}
