package utils

import org.scalacheck.Gen

object DatasetItemGen {
  def datasetItemGen: Gen[Image] = {
    for {
      index <- Gen.choose(0, Int.MaxValue)
      label <- Gen.stringOfN(5, Gen.alphaNumChar)
    } yield {
      Image(index, label)
    }
  }
}
