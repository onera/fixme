import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class mainTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  it should "compute the indicators from ets23 on for each functional equivalence classes for SA0" in {
    main.main(Array(
      "analysis", // enable the processing of campaign results
      //"-df", "(0,100)", // "filter all campaign results files "datalog_0","datalog_1",...,"datalog_9"
      //"-th", "4", // number of core to use: default use all available processors
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip,stuck_at_1",
      "-gcf", "output/logs/functionalClassesEqScoresSA0.csv",
      "-fc", "src/test/resources/failureClasses/failureClassesCNN.json",
      "-msn", "ets23-SA0", // override export file name
      //"-p", // "--plot":  enable generation of plotting files for gnuplot
      "-M", "src/test/resources/measures/ets23.json", //path to a measure file
      "data/leNet5" // the path to the directory containing all campaign related files
    ))
  }

  it should "compute the indicators from ets23 on for each functional equivalence classes for SA1" in {
    main.main(Array(
      "analysis", // enable the processing of campaign results
      //"-df", "(0,100)", // "filter all campaign results files "datalog_0","datalog_1",...,"datalog_9"
      // "-th", "16", // number of core to use: default use all available processors
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip,stuck_at_0",
      //"-gcf", "output/logs/functionalClassesEqScoresSA1.csv",
      "-msn", "ets23-SA1", // override export file name
      "-fc", "src/test/resources/failureClasses/failureClassesCNN.json",
      //"-p", // "--plot":  enable generation of plotting files for gnuplot
      "-M", "src/test/resources/measures/ets23.json", //path to a measure file
      "src/test/resources/leNet5" // the path to the directory containing all campaign related files
    ))
  }

  it should "compute the data impact for SA0" in {
    main.main(Array(
      "analysis", // enable the processing of campaign results
      //    "-df", "(0,100)", // "filter all campaign results files "datalog_0","datalog_1",...,"datalog_9"
      // "-th", "16", // number of core to use: default use all available processors
      "-v", // "--verbose": enable printing results in console
      //"-gcf", "output/logs/functionalClassesEqScoresSA0.csv",
      "-fc", "src/test/resources/failureClasses/failureClassesCNN.json", // failureClasses file
      "-ex", "stuck_at_1,bitflip", // exclude result directories containing one of these strings
      "-msn", "ets23-dataImpact-SA0", // override export file name
      //"-gcf", "output/logs/functionalClasses", // filter the injections that are functionally equivalent for all data (must be computed first)
      "-p", // "--plot":  enable generation of plotting files for gnuplot
      "-M", "src/test/resources/measures/dataImpact.json", //path to a measure file
      "src/test/resources/leNet5" // the path to the directory containing all campaign related files
    ))
  }
  it should "compute the data impact for SA1" in {
    main.main(Array(
      "analysis", // enable the processing of campaign results
      //    "-df", "(0,100)", // "filter all campaign results files "datalog_0","datalog_1",...,"datalog_9"
      // "-th", "16", // number of core to use: default use all available processors
      "-v", // "--verbose": enable printing results in console
      //"-gcf", "output/logs/functionalClassesEqScoresSA0.csv",
      "-fc", "src/test/resources/failureClasses/failureClassesCNN.json",
      "-msn", "ets23-dataImpact-SA1", // override export file name
      "-ex", "stuck_at_0,bitflip", // exclude result directories containing one of these strings
      //"-gcf", "output/logs/functionalClasses", // filter the injections that are functionally equivalent for all data (must be computed first)
      "-p", // "--plot":  enable generation of plotting files for gnuplot
      "-M", "src/test/resources/measures/dataImpact.json", //path to a measure file
      "src/test/resources/leNet5" // the path to the directory containing all campaign related files
    ))
  }
}


class mainTestFunctionalClasses extends AnyFlatSpec with should.Matchers {
  it should "compute the functional classes for SA0" in {
    main.main(Array(
      "functionalClasses", // enable the computation of equivalent classes
      "-df", "(0,1000)", // "filter the  campaign results files "datalog_0","datalog_1",...,"datalog_9"
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip,stuck_at_1", // exclude result directories containg "bitflip" or "stuck_at_0" from parsing
      "data/leNet5" // the path to the directory containing all campaign related files
    ))
  }

  it should "compute the functional classes for SA1" in {
    main.main(Array(
      "functionalClasses", // enable the computation of equivalent classes
      "-df", "(0,1000)", // "filter the  campaign results files "datalog_0","datalog_1",...,"datalog_9"
      "-v", // "--verbose": enable printing results in console
      "-ex", "bitflip,stuck_at_0", // exclude result directories containg "bitflip" or "stuck_at_0" from parsing
      "data/leNet5" // the path to the directory containing all campaign related files
    ))
  }

}
class mainTestFlows extends AnyFlatSpec with should.Matchers {
  it should "export the flows in LeNet5 streaming accelerator" in {
    main.main(Array(
      "flows", // enable the computation of flows
      "src/test/resources/leNet5/architecture.json" // the path to the cnn model json file
    ))
  }

  it should "compute the coverage of input based strategy in LeNet5 streaming accelerator" in {
    main.main(Array(
      "coverage", // enable the computation of classes
      "-is", "inputBased", // name of the injection strategy
      "src/test/resources/leNet5/architecture.json" // the path to the cnn model json file
    ))
  }
}
