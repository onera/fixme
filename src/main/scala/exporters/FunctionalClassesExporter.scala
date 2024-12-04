package exporters

import models.CampaignResult
import utils.UnionFind

trait FunctionalClassesExporter extends DataFileWriter {}


object ForallDataNumericBasedEqualityExporter {

  /**
   * Compute the quotient set Stream[Injection]/~ where l ~ r iff measure(l) = measure (r)
   *
   * @param results the initial set of injection
   * @tparam T the type of the measure
   * @return the quotient set
   */
  def apply[A, B](results: Map[B, Stream[A]]): UnionFind[A] = {
    new UnionFind(results.values.toSeq.flatten) {
      for {
        (_, c) <- results if c.size >= 2
        Seq(l, r) <- c.sliding(2)
      }
        l <=> r
    }
  }

  def apply2[A, B](results: Map[B, Stream[A]]): UnionFind[A] = {
    val result = UnionFind.empty[A]
    for {
      (_, c) <- results
    } {
      if (c.size == 1)
        result += c.head
      else {
        c.foreach(x => result += x)
        result.addEquivalenceClass(c)
      }
    }
    result
  }


}

object ScoresEqualityClassesExporter extends FunctionalClassesExporter {
  def apply(name: String, results: Iterator[CampaignResult]) = {
    val unionFind = UnionFind.empty[(String, Stream[Map[String, Double]])]
    for {
      (injectionPoint, scores) <- results.toStream.groupBy(_.injectionPoint.toString).mapValues(_.map(_.scores))
    } {
      unionFind += (injectionPoint, scores)
      for {
        c <- unionFind.equivalenceClasses
        if (c.head._2 == scores)
      }
        unionFind.<=>(c.head, (injectionPoint, scores))
    }
    unionFind
  }
}

object OrderEqualityClassesExporter extends FunctionalClassesExporter {
  def apply(name: String, results: Stream[CampaignResult]) = {
    val unionFind = UnionFind.empty[(String, Stream[List[String]])]
    for {
      (injectionPoint, ranks) <- results.groupBy(_.injectionPoint.toString).mapValues(_.map(_.scores.toList.sortBy(_._2).map(_._1)))
    } {
      unionFind += (injectionPoint, ranks)
      for {
        c <- unionFind.equivalenceClasses
        if (c.head._2 == ranks)
      }
        unionFind.<=>(c.head, (injectionPoint, ranks))
    }
    unionFind
  }
}

object ClassificationEqualityClassesExporter extends FunctionalClassesExporter {
  def apply(name: String, results: Stream[CampaignResult]) = {
    val unionFind = UnionFind.empty[(String, Stream[String])]
    for {
      (injectionPoint, classification) <- results.groupBy(_.injectionPoint.toString).mapValues(_.map(_.observedClassification.head))
    } {
      unionFind += (injectionPoint, classification)
      for {
        c <- unionFind.equivalenceClasses
        if (c.head._2 == classification)
      }
        unionFind.<=>(c.head, (injectionPoint, classification))
    }
    unionFind
  }
}


object FunctionalClassesExporter extends DataFileWriter {
  def export(name: String, results: Stream[CampaignResult]) = {
    val unionFind = UnionFind.empty[(String, Stream[Map[String, Double]])]
    for {
      (injectionPoint, scores) <- results.groupBy(_.injectionPoint.toString).mapValues(_.map(_.scores))
    } {
      unionFind += (injectionPoint, scores)
      for {
        c <- unionFind.equivalenceClasses
        if (c.head._2 == scores)
      }
        unionFind.<=>(c.head, (injectionPoint, scores))
    }

    val equClasses = unionFind.equivalenceClasses
    val sizesEqScore = equClasses.groupBy(_.size)
    val exportData = equClasses.map(p => (p.head._1, Map("size" -> p.size))).toMap
    writeLogFile(name + "EqScores.csv", exportData, List("classRepr", "size"))

    for {
      cL <- unionFind.equivalenceClasses
      repL = cL.head
    }
      for {
        cR <- unionFind.equivalenceClasses
        reprR = cR.head
      }
        if (reprR._2.zip(repL._2).forall { scores => scores._2.toList.sortBy(_._2).map(_._1) == scores._1.toList.sortBy(_._2).map(_._1) })
          unionFind.<=>(repL, reprR)

    val equClassesOrderPreserved = unionFind.equivalenceClasses
    val exportData2 = equClassesOrderPreserved.map(p => (p.head._1, Map("size" -> p.size))).toMap
    writeLogFile(name + "SameOrder.csv", exportData2, List("classRepr", "size"))

    for {
      cL <- unionFind.equivalenceClasses
      repL = cL.head
    }
      for {
        cR <- unionFind.equivalenceClasses
        reprR = cR.head
      }
        if (reprR._2.zip(repL._2).forall { scores => scores._2.maxBy(_._2)._1 == scores._1.toList.sortBy(_._2).maxBy(_._2)._1 })
          unionFind.<=>(repL, reprR)

    val equClassesClassifPreserved = unionFind.equivalenceClasses
    val exportData3 = equClassesClassifPreserved.map(p => (p.head._1, Map("size" -> p.size))).toMap
    writeLogFile(name + "SameClassif.csv", exportData3, List("classRepr", "size"))
    unionFind
  }

  def export(name: String, results: Seq[Stream[CampaignResult]]) = {
    val unionFind = results.par.map({ res =>
      val unionFind = UnionFind.empty[(String, Stream[Map[String, Double]])]
      for {
        (injectionPoint, scores) <- res.groupBy(_.injectionPoint.toString).mapValues(_.map(_.scores))
      } {
        unionFind += (injectionPoint, scores)
        for {
          c <- unionFind.equivalenceClasses
          if (c.head._2 == scores)
        }
          unionFind.<=>(c.head, (injectionPoint, scores))
      }
      unionFind
    }).reduce {
      (acc, uf) =>
        if (!acc.elements.exists(e => uf.elements.exists(p => p._1.matches(e._1))))
          acc || uf
        else {
          val unionFind = UnionFind.empty[(String, Stream[Map[String, Double]])]
          for {
            classAcc <- acc.equivalenceClasses
          }
            for {
              element <- classAcc
            } {
              unionFind += element // if (uf.elements.contains())
              if (uf.<=>?(element, classAcc.head))
                unionFind.<=>(element, classAcc.head)
            }
          unionFind
        }
    }
    val equClassesClassifPreserved = unionFind.equivalenceClasses
    val exportData = equClassesClassifPreserved.map(p => (p.head._1, Map("size" -> p.size))).toMap
    writeLogFile(name + "ParScoresPreserved.csv", exportData, List("classRepr", "size"))
    unionFind
  }
}
