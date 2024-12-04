package indicators

import models.ClassifiedResult

trait InjectionImpact {
  val name: String
  val exists: Stream[ClassifiedResult] => Boolean
  val existsNone: Stream[ClassifiedResult] => Boolean

  def apply(m: Stream[ClassifiedResult]): (Boolean, Boolean) = (existsNone(m), exists(m))
}


case class otherInjectionImpact(name: String, existsNone: Stream[ClassifiedResult] => Boolean, exists: Stream[ClassifiedResult] => Boolean) extends InjectionImpact