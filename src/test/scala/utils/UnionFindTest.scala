package utils

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class UnionFindTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  val uniquePositiveInts: Gen[List[Int]] =
    Gen.containerOf[Set, Int](Gen.posNum[Int])
      .map(_.toList)
      .map(_.sorted)

  "Union find" should "encode equivalence classes" in {
    forAll((uniquePositiveInts, "union")) {
      union => whenever (union.nonEmpty) {
        val unionFind = new UnionFind[Int](union)
        import unionFind._
        union.filter(_ % 2 == 0).sliding(2).foreach(l => l.head <=> l.last)
        for (List(a,b) <- union.filter(_ % 2 == 0).sliding(2)) {
          a <=>? b shouldBe true
        }
      }
    }
  }

  "Union find" should "remove properly values" in {
    val unionFind = new UnionFind(Array("a","b","c","d","e","f","g")) {
      "a" <=> "b"
      "a" <=> "c"
      "b" <=> "d"
      "e" <=> "f"
    }
    import unionFind._

    unionFind discard "b"
    "a" <=>? "d" shouldBe true
    unionFind.elements.contains("b") shouldBe false
  }

  "Union find" should "be merged properly" in {
    val left = new UnionFind(Array("a", "b", "c", "d", "e", "f", "g")) {
      "a" <=> "b"
      "a" <=> "c"
      "b" <=> "d"
      "e" <=> "f"
    }
    val right = new UnionFind(Array("a", "e", "f", "g", "h", "i")) {
      "a" <=> "e"
      "h" <=> "i"
    }
    //    val or = right || left

    val and = right && left

    //    or.<=>?("a", "b") shouldBe true

    for {c <- and.equivalenceClasses}
      c.size shouldBe 1
  }

}
