package exporters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import utils.FaultType.{BITFLIP, STUCK_AT_0, STUCK_AT_1}



class ObservedMeasureExporterTest extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "Observed measures" should "be computed on SA-0 for some data" in
    computeMeasure(toObservedMeasures, "ETSObservedMeasuresSA0.csv", STUCK_AT_0, fullTest = false)

  "Observed measures" should "be computed on SA-1 for some data" in
    computeMeasure(toObservedMeasures, "ETSObservedMeasuresSA1.csv", STUCK_AT_1, fullTest = false)

  "Observed measures" should "be computed on BF for some data" in
    computeMeasure(toObservedMeasures, "ETSObservedMeasuresBF.csv", BITFLIP, fullTest = false)

}

class ObservedMeasureExporterTestGlobal extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "Observed measures" should "be computed on SA-0 for all data" in
    computeMeasure(toObservedMeasures, "ETSObservedMeasuresSA0.csv", STUCK_AT_0, fullTest = true)

  "Observed measures" should "be computed on SA-1 for all data" in
    computeMeasure(toObservedMeasures, "ETSObservedMeasuresSA1.csv", STUCK_AT_1, fullTest = true)

  "Observed measures" should "be computed on BF for all data" in
    computeMeasure(toObservedMeasures, "ETSObservedMeasuresBF.csv", BITFLIP, fullTest = true)
}


class MisclassificationExtendedMeasureExporterTest extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "MisclassificationExtended measures" should "be computed on SA-0 for some data" in
    computeMeasure(toMisclassificationExtendedMeasures, "ETSMisclassificationExtendedMeasuresSA0.csv", STUCK_AT_0, fullTest = false)

  "MisclassificationExtended measures" should "be computed on SA-1 for some data" in
    computeMeasure(toMisclassificationExtendedMeasures, "ETSMisclassificationExtendedMeasuresSA1.csv", STUCK_AT_1, fullTest = false)

  "MisclassificationExtended measures" should "be computed on BF for some data" in
    computeMeasure(toMisclassificationExtendedMeasures, "ETSMisclassificationExtendedMeasuresBF.csv", BITFLIP, fullTest = false)

}

class MisclassificationExtendedGlobalMeasureExporterTest extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "MisclassificationExtended measures" should "be computed on SA-0 for all data" in
    computeMeasure(toMisclassificationExtendedMeasures, "ETSMisclassificationExtendedMeasuresSA0.csv", STUCK_AT_0, fullTest = true)

  "MisclassificationExtended measures" should "be computed on SA-1 for all data" in
    computeMeasure(toMisclassificationExtendedMeasures, "ETSMisclassificationExtendedMeasuresSA1.csv", STUCK_AT_1, fullTest = true)

  "MisclassificationExtended measures" should "be computed on BF for all data" in
    computeMeasure(toMisclassificationExtendedMeasures, "ETSMisclassificationExtendedMeasuresBF.csv", BITFLIP, fullTest = true)

}


class MisClassificationMeasureExporterTest extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "MisClassification measures" should "be computed on SA-0 for some data" in
    computeMeasure(toMisClassificationMeasures, "ETSMisClassificationMeasuresSA0.csv", STUCK_AT_0, fullTest = false)

  "MisClassification measures" should "be computed on SA-1 for some data" in
    computeMeasure(toMisClassificationMeasures, "ETSMisClassificationMeasuresSA1.csv", STUCK_AT_1, fullTest = false)

  "MisClassification measures" should "be computed on BF for some data" in
    computeMeasure(toMisClassificationMeasures, "ETSMisClassificationMeasuresBF.csv", BITFLIP, fullTest = false)

}

class MisClassificationMeasureExporterTestGlobal extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "MisClassification measures" should "be computed on SA-0 for all data" in
    computeMeasure(toMisClassificationMeasures, "ETSMisClassificationMeasuresSA0.csv", STUCK_AT_0, fullTest = true)

  "MisClassification measures" should "be computed on SA-1 for all data" in
    computeMeasure(toMisClassificationMeasures, "ETSMisClassificationMeasuresSA1.csv", STUCK_AT_1, fullTest = true)

  "MisClassification measures" should "be computed on BF for all data" in
    computeMeasure(toMisClassificationMeasures, "ETSMisClassificationMeasuresBF.csv", BITFLIP, fullTest = true)
}


class MisClassificationMeasureMapExporterTest extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "MisClassification measures" should "be computed on SA-0 for some data" in
    computeMeasureMap(toMisClassificationCountMeasures, "ETSMisClassificationMeasuresMapSA0.csv", STUCK_AT_0, fullTest = false)

  "MisClassification measures" should "be computed on SA-1 for some data" in
    computeMeasureMap(toMisClassificationCountMeasures, "ETSMisClassificationMeasuresMapSA1.csv", STUCK_AT_1, fullTest = false)

  "MisClassification measures" should "be computed on BF for some data" in
    computeMeasureMap(toMisClassificationCountMeasures, "ETSMisClassificationMeasuresMapBF.csv", BITFLIP, fullTest = false)

}


class MisClassificationMeasureMapGlobalExporterTest extends AnyFlatSpec with should.Matchers with StreamBasedMeasure {

  "MisClassification measures" should "be computed on SA-0 for all data" in
    computeMeasureMap(toMisClassificationCountMeasures, "ETSMisClassificationMeasuresMapSA0.csv", STUCK_AT_0, fullTest = true)

  "MisClassification measures" should "be computed on SA-1 for all data" in
    computeMeasureMap(toMisClassificationCountMeasures, "ETSMisClassificationMeasuresMapSA1.csv", STUCK_AT_1, fullTest = true)

  "MisClassification measures" should "be computed on BF for all data" in
    computeMeasureMap(toMisClassificationCountMeasures, "ETSMisClassificationMeasuresMapBF.csv", BITFLIP, fullTest = true)

}