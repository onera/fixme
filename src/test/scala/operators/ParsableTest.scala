package operators

import models.DataMeasures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import utils.FileManager.getExistingCsvFile
import operators.all._

import scala.io.Source

class ParsableTest extends AnyFlatSpec with should.Matchers {
  "DataMeasures" should "be parsable" in {
    for {
      file <- getExistingCsvFile("dataMeasures-fullSA0")
    } yield {
      val s = Source.fromFile(file)
      val m = s.parse[Stream[DataMeasures]]
      val mod = m.map(x => x.copy(countXMisc = x.countXMisc.zipWithIndex.map(_._2)))

      val fileModif = "testParsingDataMeasures".export(mod).get

      val s2 = Source.fromFile(fileModif)
      val modif = s2.parse[Stream[DataMeasures]]
      s2.close()
      s.close()
    }
  }
}
