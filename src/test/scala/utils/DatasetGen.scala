package utils

import org.scalacheck.Gen
import utils.Dataset.{Dataset, MNIST}

object DatasetGen {
  /**
   * Define a way to generate a random DataSet from possible ones (need to be completed)
   */
  val datasetGen: Gen[Dataset] = Gen.frequency(
    1 -> MNIST
  )
}
