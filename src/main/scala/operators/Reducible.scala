package operators

import models._

trait Reducible[T] {
  @inline def reduce(left: T, right: T): T
}


object Reducible {

  trait Instances {
    implicit def equivalenceClassesLikeIsReducible[T <: EquivalenceClasses](implicit equivalenceBuildable: EquivalenceBuildable[T]): Reducible[T] =
      (left: T, right: T) =>
        equivalenceBuildable.buildFromClasses(for {
          lc <- left.classes
          rc <- right.classes
          nc = lc.intersect(rc) if nc.size >= 2
        } yield {
          nc
        })


    implicit object ScoreEqualityEquivalenceClassesMapIsReducible extends Reducible[ScoreEqualityEquivalenceClassesMap] {
      def reduce(left: ScoreEqualityEquivalenceClassesMap, right: ScoreEqualityEquivalenceClassesMap): ScoreEqualityEquivalenceClassesMap =
        ScoreEqualityEquivalenceClassesMap((for {
          key <- left.classes.keySet intersect right.classes.keySet
          intersection = left.classes(key) intersect right.classes(key)
          if intersection.size >= 2
        } yield {
          key -> intersection
        }).toMap)
    }

    implicit object GlobalMeasuresIsReducible extends Reducible[GlobalMeasures] {
      def reduce(left: GlobalMeasures, right: GlobalMeasures): GlobalMeasures =
        GlobalMeasures(
          left.atLeastOneScoreModif || right.atLeastOneScoreModif,
          left.alwaysPreserveClassif && right.alwaysPreserveClassif,
          left.alwaysTopScoreDegradedLeq.zip(right.alwaysTopScoreDegradedLeq).map(p => p._1 && p._2),
          left.alwaysScoreModifiedLeq.zip(right.alwaysScoreModifiedLeq).map(p => p._1 && p._2),
          left.alwaysRankModifiedLeq.zip(right.alwaysRankModifiedLeq).map(p => p._1 && p._2),
          left.alwaysSoftMaxDegradedLeq.zip(right.alwaysSoftMaxDegradedLeq).map(p => p._1 && p._2),
          left.maxRankDowngrade max right.maxRankDowngrade
        )
    }


    implicit def measuresStreamLikeIsReducible[T <: Measures](implicit reducible: Reducible[T]): Reducible[Stream[T]] =
      (left: Stream[T], right: Stream[T]) => {
        left.zip(right).map(p => reducible.reduce(p._1, p._2))
      }


    implicit object DataMeasuresIsReducible extends Reducible[DataMeasures] {
      def reduce(left: DataMeasures, right: DataMeasures): DataMeasures = {
        DataMeasures(
          if (left.point.matches(right.point)) left.point else "error",
          left.countXMisc.zip(right.countXMisc).map(p => p._2 + p._1),
          left.countMisc + right.countMisc,
          left.countObserved + right.countObserved,
          left.countMasked + right.countMasked
        )
      }
    }

    implicit object AccuracyMeasuresIsReducible extends Reducible[AccuracyMeasures] {
      def reduce(left: AccuracyMeasures, right: AccuracyMeasures): AccuracyMeasures = {
        AccuracyMeasures(
          if (left.point.matches(right.point)) left.point else "error",
          left.countMissGold + right.countMissGold,
          left.countMissFaulty + right.countMissFaulty,
          left.countTotal + right.countTotal
        )
      }
    }

    implicit object InjectionMeasuresIsReducible extends Reducible[Stream[InjectionMeasures]] {
      def reduce(left: Stream[InjectionMeasures], right: Stream[InjectionMeasures]): Stream[InjectionMeasures] = {
        left ++ right
      }
    }
  }
}
