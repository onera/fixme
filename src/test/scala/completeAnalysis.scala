import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class completeAnalysis extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {
  it should "compute the functional classes for any SA" in {
    main.main(Array(
      "functionalClasses", // enable the computation of equivalent classes
      "-df", "(0,1000)", // "filter the  campaign results files "datalog_0","datalog_1",...,"datalog_9"
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip", // exclude result directories containg "bitflip"
      "src/test/resources/leNet5" // the path to the directory containing all campaign related files
    ))
  }

  it should "compute the indicators from ets23 on for each functional class" in {
    main.main(Array(
      "analysis", // enable the processing of campaign results
      "-df", "(0,10)", // "filter all campaign results files "datalog_0","datalog_1",...,"datalog_9"
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip",
      //"-gcf", "output/logs/functionalClassesEqScores.csv", // pass a functional classes list (must be computed first)
      "-fc","src/test/resources/failureClasses/failureClassesCNN.json",
      "-p", // "--plot":  enable generation of plotting files for gnuplot
      "-M", "src/test/resources/measures/ets23.json", //path to a measure file
      "src/test/resources/leNet5" // the path to the directory containing all campaign related files
    ))
  }

  it should "compute the indicators from ets23 for each injection point SA" in {
    main.main(Array(
      "analysis", // enable the processing of campaign results
      "-df", "(0,1000)", // "filter all campaign results files "datalog_0","datalog_1",...,"datalog_9"
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip",
      //"-p", // "--plot":  enable generation of plotting files for gnuplot
      "-M", "src/test/resources/measures/ets23.json", //path to a measure file
      "data/leNet5" // the path to the directory containing all campaign related files
    ))
  }
}
