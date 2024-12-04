package utils

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.Dataset.MNIST

import java.io.{File, FileWriter}
import java.nio.file.Paths
import scala.io.Source
import scala.util.hashing.MurmurHash3

/**
 * Tests over the [[FileManager]]
 */
class FileManagerTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  /**
   * Simple test where we try to get the a given plot file
   */
  "A File Manager" should "provides a diagram file in output/plot directory with correct extension and name" in {
    val plotName = "folder/some test diagram"
    val expected = Paths.get(s"output/plot/folder_some_test_diagram.png").toAbsolutePath
    FileManager.getDiagramPlotFile(plotName) match {
      case Some(value) => assert(value.toPath === expected)
      case None => fail()
    }
  }

  /**
   * Simple test where we try to get the path of a given measure
   */
  it should "provides a data path in output/measures directory with correct extension and name" in {
    val dataName = "folder/some test diagram"
    val expected = Paths.get(s"output/plot/data/folder_some_test_diagram.dat").toAbsolutePath
    FileManager.createMeasureFilePath(dataName) match {
      case Some(value) => assert(value === expected)
      case None => fail()
    }
  }

  /**
   * more complex test where we try to write then read several measure files
   */
  it should "be able to write a string in at least 100 given measure files then read them" in {
    forAll((Gen.alphaStr, "measureFileName"), (Gen.alphaNumStr, "content")) {
      (dataName, content) => {
        FileManager.getMeasureFileWriter(dataName) match {
          case Some(fw) => {
            try {
              fw.write(content)
            }
            catch {
              case e: java.io.IOException => fail()
            } finally {
              fw.close()
            }
          }
          case None => fail()
        }
        FileManager.getExistingMeasureFile(dataName) match {
          case Some(file) => {
            val s = Source.fromFile(file)
            val read = s.getLines().mkString("\n")
            s.close()
            file.delete() //cleanup generated file
            assert(read === content)
          }
          case None => fail()
        }

      }
    }
  }


  /**
   * another test where we try to write then read several diagram files
   */
  it should "be able to write a string in at least 100 given plot diagram files then read them" in {
    forAll((Gen.alphaNumStr, "measureFileName"), (Gen.alphaNumStr, "content")) {
      (dataName, content) => {
        FileManager.getDiagramPlotFile(dataName) match {
          case Some(f) => {
            try {
              val fw = new FileWriter(f)
              fw.write(content)
              fw.close()
            }
            catch {
              case e: java.io.IOException => fail()
            }
          }
          case None => fail()
        }
        FileManager.getDiagramPlotFile(dataName) match {
          case Some(file) => {
            val s = Source.fromFile(file)
            val read = s.getLines().mkString("\n")
            s.close()
            file.delete() //cleanup generated file
            assert(read === content)
          }
          case None => fail()
        }

      }
    }
  }
  it should "find all campaign mandatory files for LeNet5" in {
    FileManager.getCampaignFiles(new File("src/test/resources/leNet5"), MNIST) match {
      case Some(value) =>
        assert(value.resultsFiles.nonEmpty && !value.resultsFiles.exists(p => p._2.isEmpty))
      case None => fail()
    }
  }

}

class HashFileNameTest extends AnyFlatSpec with should.Matchers {
  "MurmurHash3" should "produce a hash from a measure name" in {
    val name = "this is another measure name"
   println(MurmurHash3.stringHash(name))
  }
}
