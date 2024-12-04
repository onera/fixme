package operators

import models.LeNetFixture._
import models._
import operators.Fallible._
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.FileManager.locateTempFile
import utils.UnionFind

import scala.io.Source

class FallibleTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  def bitwidthPerNameInConv(s: String): Int =
    if (s.contains("dotProduct") && s.contains("prodRegister")) 24
    else if (s.contains("dotProduct") && s.contains("prod") && s.contains(".i")) 16
    else if (s.contains("dotProduct") && s.contains("prod") && s.contains(".o")) 24
    else if (s.contains("dotProduct") && s.contains("add.i")) 24
    else if (s.contains("dotProduct") && s.contains("add.o")) 32
    else if (s.contains("dotProduct") && s.contains("addRegister")) 32
    else if (s.contains("dotProduct") && s.contains("max")) 32
    else if (s.contains("extractor")) 16
    else 0

  def bitwidthPerNameInDense(s: String): Int =
    if (s.contains("outRegister")) 16
    else if (s.contains("prodRegister")) 24
    else if (s.contains("prod") && s.contains(".o")) 24
    else if (s.contains("prod") && s.contains(".i")) 16
    else if (s.contains("add.i_acc")) 32
    else if (s.contains("add.i")) 24
    else if (s.contains("add.o")) 32
    else if (s.contains("accRegister")) 32
    else if (s.contains("biasAdd")) 32
    else if (s.contains("outRegister")) 16
    else 0

  def bitwidthPerNameInPooling(s: String): Int =
    if (s.contains("Extractor") || s.contains("max")) 16
    else 0

  def bitwidthPerNameInSequencer(s: String): Int =
    if (s.contains(".r")) 16
    else 0

  def stuckAtLeNetAnalysis(network: ConvolutionalNeuralNetwork): Seq[(Int, (Int, Int))] = {
    val unionFind = network.stuckAtXClasses
    val discard = unionFind.elements.filter(id => network.layers.exists(l => id.contains(s"${l.name}.i") || id.contains(s"${l.name}.o")))
    discard.foreach(unionFind.discard)
    val summarizedClasses = unionFind.equivalenceClasses.toSeq
      .groupBy(s => s.size)
      .transform((_, classes) => {
        val SAX = classes.map(eqClass => eqClass.toSeq.map(f =>
          (for {l <- network.layers.find(x => f.contains(x.name))} yield {
            l match {
              case _: Convolution2D => bitwidthPerNameInConv(f)
              case _: Pooling2D => bitwidthPerNameInPooling(f)
              case _: Dense => bitwidthPerNameInDense(f)
              case _: Sequencer => bitwidthPerNameInSequencer(f)
              case _: Input2D => 0
            }
          }) getOrElse {
            println(s"$f not in any layer");
            0
          }))
        (SAX.map(_.max).sum, SAX.map(_.sum).sum)
      })
      .toSeq
      .sortBy(_._1)

    println(summarizedClasses.map(kv => s"${kv._2._2} SA in equivalence classes of size ${kv._1} that can be covered with ${kv._2._1} injections")
      .mkString(s"Exhaustive SA: ${summarizedClasses.map(_._2._2).sum}\nmiminal SA to be injected: ${summarizedClasses.map(_._2._1).sum}\nDetails:\n\t", "\n\t", ""))

    summarizedClasses
  }

  def bitFlipLeNetAnalysis(network: ConvolutionalNeuralNetwork): Seq[(Int, (Int, Int))] = {
    val unionFind = network.bitFlipClasses(mkLeNetInputTFlow(network.input2D))
    val discard = unionFind.elements.filter(id => network.layers.exists(l => id.contains(s"${l.name}.i") || id.contains(s"${l.name}.o")))
    discard.foreach(unionFind.discard)
    val summarizedClasses = unionFind.equivalenceClasses.toSeq
      .groupBy(s => s.size)
      .transform((_, v) => {
        val BF = v.map(s => s.toSeq.map(f =>
          List(bitwidthPerNameInDense(f),
            bitwidthPerNameInConv(f),
            bitwidthPerNameInPooling(f),
            bitwidthPerNameInSequencer(f)).max))
        (BF.map(_.max).sum, BF.map(_.sum).sum)
      })
      .toSeq
      .sortBy(_._1)

    println(summarizedClasses.map(kv => s"${kv._2._2} BF in equivalence classes of size ${kv._1} that can be covered with ${kv._2._1} injections")
      .mkString(s"Exhaustive BF: ${summarizedClasses.map(_._2._2).sum}\nmiminal BF to be injected: ${summarizedClasses.map(_._2._1).sum}\nDetails:\n\t", "\n\t", ""))

    summarizedClasses
  }

  "The number of stuck at X in a convolution layer" should "be consistent for the custom example" in {
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val expectedREG16 = cin * (win * (hker - 1) + wker)
    val expectedREG24 = cin * hker * wker * cout
    val expectedREG32 = cout
    val expectedProd = cin * hker * wker * cout
    val expectedAdd = cout
    val expectedMax = cout

    result.stuckAtCount shouldBe expectedREG16 * stuckAtXRegister(16) +
      expectedREG24 * stuckAtXRegister(24) +
      expectedREG32 * stuckAtXRegister(32) +
      expectedProd * stuckAtXComb(Array(16), 24) +
      expectedAdd * stuckAtXComb(cin * hker * wker, 24, 32) +
      expectedMax * stuckAtXComb(1, 32)

    result.stuckAtXClasses.elements.map(bitwidthPerNameInConv).sum shouldBe result.stuckAtCount
  }
  //TODO: ajouter le port de sortie du dernier registre dans la classes d'équivalence (port d'entrée du premier registre)= cf fan-out/fan-in
  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.conv1.stuckAtCount shouldBe 22016
    LeNetFixtureSafecompPaper.conv1.stuckAtXClasses.elements.map(bitwidthPerNameInConv).sum shouldBe 22016
    LeNetFixtureSafecompPaper.conv2.stuckAtCount shouldBe 283072
    LeNetFixtureSafecompPaper.conv2.stuckAtXClasses.elements.map(bitwidthPerNameInConv).sum shouldBe 283072
  }

  "The number of stuck at X in a pooling layer" should "be consistent for custom example" in {
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val expectedREG16 = cin * (win * (hpool - 1) + 1 + wpool)
    val expectedMax = cin * (stuckAtXComb(hpool, 16, 16) + stuckAtXComb(wpool, 16, 16))
    result.stuckAtCount shouldBe expectedREG16 * stuckAtXRegister(16) + expectedMax

    result.stuckAtXClasses.elements.map(bitwidthPerNameInPooling).sum shouldBe result.stuckAtCount
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.pool1.stuckAtCount shouldBe 6528
    LeNetFixtureSafecompPaper.pool1.stuckAtXClasses.elements.map(bitwidthPerNameInPooling).sum shouldBe 6528
    LeNetFixtureSafecompPaper.pool2.stuckAtCount shouldBe 8192
    LeNetFixtureSafecompPaper.pool2.stuckAtXClasses.elements.map(bitwidthPerNameInPooling).sum shouldBe 8192
  }

  "The number of stuck at X in a sequencer" should "be consistent for custom example" in {
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val expectedREG16 = cin
    result.stuckAtCount shouldBe expectedREG16 * stuckAtXRegister(16)

    result.stuckAtXClasses.elements.map(bitwidthPerNameInSequencer).sum shouldBe result.stuckAtCount
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.seq1.stuckAtCount shouldBe 3840
    LeNetFixtureSafecompPaper.seq1.stuckAtXClasses.elements.map(bitwidthPerNameInSequencer).sum shouldBe 3840
    LeNetFixtureSafecompPaper.seq2.stuckAtCount shouldBe 2688
    LeNetFixtureSafecompPaper.seq2.stuckAtXClasses.elements.map(bitwidthPerNameInSequencer).sum shouldBe 2688
  }

  "The number of stuck at X in a dense layer" should "be consistent for custom example" in {
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val expectedREG16 = cout * stuckAtXRegister(16)
    val expectedREG24 = cin * cout * stuckAtXRegister(24)
    val expectedREG32 = cout * stuckAtXRegister(32)
    val expectedProd = cout * cin * stuckAtXComb(Array(16), 24)
    val expectedAdd = cout * stuckAtXComb(Array.fill(cin)(24) :+ 32, 32)
    val expectedBias = cout * stuckAtXComb(1, 32)
    result.stuckAtCount shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedAdd + expectedProd + expectedBias

    val stuckAtClasses = result.stuckAtXClasses

    stuckAtClasses.elements.filter(_.contains("outRegister")).map(bitwidthPerNameInDense).sum shouldBe expectedREG16
    stuckAtClasses.elements.filter(_.contains("prodRegister")).map(bitwidthPerNameInDense).sum shouldBe expectedREG24
    stuckAtClasses.elements.filter(_.contains("accRegister")).map(bitwidthPerNameInDense).sum shouldBe expectedREG32
    stuckAtClasses.elements.filter(s => s.contains("prod") && !s.contains("prodRegister") && (s.contains(".o") || s.contains(".i"))).map(bitwidthPerNameInDense).sum shouldBe expectedProd
    stuckAtClasses.elements.filter(_.contains("add")).map(bitwidthPerNameInDense).sum shouldBe expectedAdd
    stuckAtClasses.elements.filter(_.contains("biasAdd")).map(bitwidthPerNameInDense).sum shouldBe expectedBias
    stuckAtClasses.elements.map(bitwidthPerNameInDense).sum shouldBe result.stuckAtCount
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.fc1.stuckAtCount shouldBe 241920
    LeNetFixtureSafecompPaper.fc1.stuckAtXClasses.elements.map(bitwidthPerNameInDense).sum shouldBe 241920
    LeNetFixtureSafecompPaper.fc2.stuckAtCount shouldBe 112896
    LeNetFixtureSafecompPaper.fc2.stuckAtXClasses.elements.map(bitwidthPerNameInDense).sum shouldBe 112896
    LeNetFixtureSafecompPaper.fc3.stuckAtCount shouldBe 25760
    LeNetFixtureSafecompPaper.fc3.stuckAtXClasses.elements.map(bitwidthPerNameInDense).sum shouldBe 25760
  }

  "The total number of stuck at X" should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.stuckAtCount shouldBe 706912
    stuckAtLeNetAnalysis(LeNetFixtureSafecompPaper).map(_._2._2).sum shouldBe 706912
  }

  "The number of bitflips in a convolution layer" should "be consistent for the custom example" in {
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val transformed = result.transform(LeNetFixtureSafecompPaper.customInput)
    val expectedREG16 = 16 * cin * transformed.neighbourExtractor.registers.map(r => vulnerableCount(r.output.tFlow)).sum
    val expectedREG24 = cin * hker * wker * cout * 24 * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
    val expectedREG32 = cout * 32 * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)

    result.bitFlipsCount(LeNetFixtureSafecompPaper.customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32
    val classes = result.bitFlipClasses(LeNetFixtureSafecompPaper.customInput)
    classes.elements.filter(_.contains("extractor")).map(bitwidthPerNameInConv).sum shouldBe expectedREG16
    classes.elements.filter(_.contains("prod")).map(bitwidthPerNameInConv).sum shouldBe expectedREG24
    classes.elements.filter(_.contains("add")).map(bitwidthPerNameInConv).sum shouldBe expectedREG32
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.conv1.bitFlipsCount(LeNetFixtureSafecompPaper.imageInput) shouldBe 4983040
    LeNetFixtureSafecompPaper.conv1.bitFlipClasses(LeNetFixtureSafecompPaper.imageInput).elements.map(bitwidthPerNameInConv).sum shouldBe 4983040
    //   LeNetFixtureSafecompPaper.conv2.bitFlips(LeNetFixture.pool2Output) shouldBe 145344
    //   LeNetFixtureSafecompPaper.conv2.bitFlipClasses(LeNetFixture.pool2Output).elements.map(bitwidthPerNameInConv).sum shouldBe 145344
  }

  "The number of bitflips in a pooling layer" should "be consistent for custom example" in {
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val transformed = result.transform(LeNetFixtureSafecompPaper.customInput)
    val expectedREG16 = 16 * cin * transformed.registers.map(r => vulnerableCount(r.output.tFlow)).sum
    result.bitFlipsCount(LeNetFixtureSafecompPaper.customInput) shouldBe expectedREG16
    val classes = result.bitFlipClasses(LeNetFixtureSafecompPaper.customInput)
    val classesBySize = classes.equivalenceClasses.toSeq.groupBy(_.size)
    classes.elements.count(_.contains(".r")) * 16 shouldBe expectedREG16
    print("ok")
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.pool1.bitFlipsCount(LeNetFixtureSafecompPaper.conv1Output) shouldBe 1185408
    LeNetFixtureSafecompPaper.pool1.bitFlipClasses(LeNetFixtureSafecompPaper.conv1Output).elements.map(bitwidthPerNameInPooling).sum shouldBe 1185408
    LeNetFixtureSafecompPaper.pool2.bitFlipsCount(LeNetFixtureSafecompPaper.conv2Output) shouldBe 172800
    LeNetFixtureSafecompPaper.pool2.bitFlipClasses(LeNetFixtureSafecompPaper.conv2Output).elements.map(bitwidthPerNameInPooling).sum shouldBe 172800
  }

  "The number of bitflips in a sequencer" should "be consistent for custom example" in {
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val transformed = result.transform(Stream(0))
    val expectedREG16 = 16 * cout * transformed.registers.map(r => vulnerableCount(r.output.tFlow)).sum
    result.bitFlipsCount(Stream(0)) shouldBe expectedREG16
    val classes = result.bitFlipClasses(Stream(0))
    classes.elements.count(_.contains(".r")) * 16 shouldBe expectedREG16
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.seq1.bitFlipsCount(LeNetFixtureSafecompPaper.fc1Output) shouldBe 12480
    LeNetFixtureSafecompPaper.seq1.bitFlipClasses(LeNetFixtureSafecompPaper.fc1Output).elements.count(_.contains(".r")) * 16 shouldBe 12480
    LeNetFixtureSafecompPaper.seq2.bitFlipsCount(LeNetFixtureSafecompPaper.fc2Output) shouldBe 3360
    LeNetFixtureSafecompPaper.seq2.bitFlipClasses(LeNetFixtureSafecompPaper.fc1Output).elements.count(_.contains(".r")) * 16 shouldBe 3360
  }

  "The number of bitflips in a dense layer" should "be consistent for custom example" in {
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val f = Stream(2)
    val transformed = result.transform(f)
    val expectedREG16 = cout * 16 * vulnerableCount(transformed.outRegister.output.tFlow)
    val expectedREG24 = cin * cout * 24 * vulnerableCount(transformed.prodRegister.output.tFlow)
    val expectedREG32 = cout * 32 * transformed.accRegister.outputs.map(o => vulnerableCount(o.tFlow)).sum
    result.bitFlipsCount(f) shouldBe expectedREG16 + expectedREG24 + expectedREG32
    val classes = result.bitFlipClasses(f)
    classes.elements.filter(_.contains("prodRegister")).map(bitwidthPerNameInDense).sum shouldBe expectedREG24
    classes.elements.filter(_.contains("accRegister")).map(bitwidthPerNameInDense).sum shouldBe expectedREG32
    classes.elements.filter(_.contains("outRegister")).map(bitwidthPerNameInDense).sum shouldBe expectedREG16
  }

  it should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.fc1.bitFlipsCount(LeNetFixtureSafecompPaper.pool2Output) shouldBe 1249920
    LeNetFixtureSafecompPaper.fc1.bitFlipClasses(LeNetFixtureSafecompPaper.pool2Output).elements.map(bitwidthPerNameInDense).sum shouldBe 1249920
    LeNetFixtureSafecompPaper.fc2.bitFlipsCount(LeNetFixtureSafecompPaper.seq1Output) shouldBe 275520
    LeNetFixtureSafecompPaper.fc2.bitFlipClasses(LeNetFixtureSafecompPaper.seq1Output).elements.map(bitwidthPerNameInDense).sum shouldBe 275520
    LeNetFixtureSafecompPaper.fc3.bitFlipsCount(LeNetFixtureSafecompPaper.seq2Output) shouldBe 21600
    LeNetFixtureSafecompPaper.fc3.bitFlipClasses(LeNetFixtureSafecompPaper.seq2Output).elements.map(bitwidthPerNameInDense).sum shouldBe 21600
  }

  "The total number of bitflips" should "consistent for leNet5" in {
    LeNetFixtureSafecompPaper.bitFlipsCount(LeNetFixtureSafecompPaper.imageInput) shouldBe 14661504
    bitFlipLeNetAnalysis(LeNetFixtureSafecompPaper).map(_._2._2).sum shouldBe 14661504
  }

  "The equivalence class analysis" should "be consistent for simple leNet5" in {
    val neuralNetwork = mkSimpleLeNet(Input2D("input", Shape2D(16,16),Shape2D(16,16)))
    stuckAtLeNetAnalysis(neuralNetwork)
    bitFlipLeNetAnalysis(neuralNetwork)
  }

  "The equivalence class analysis" should "be consistent for leNet5" in {
    stuckAtLeNetAnalysis(LeNetFixtureSafecompPaper)
    bitFlipLeNetAnalysis(LeNetFixtureSafecompPaper)
  }

  "The set of stuck-at scenarios of a pooling layer" should "be consistent for LeNet5" in {
    val pooling2D = Pooling2D("pool", Shape2D(4, 4), Shape2D(2, 2), Shape2D(2, 2), true)
    val sax = pooling2D.stuckAtScenarios
    val t = pooling2D.transform(Stream.empty[Int])
    println(sax.size)
  }
}

class FallibleManuscriptTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {
  val inputSmall = Stream(1, 4, 6, 7, 8, 9, 11, 12, 14)
  val inputLenet = (1 to 32 * 32).toStream

  def bitwidthPerNameInConv(s: String): Int =
    if (s.contains("dotProduct") && s.contains("prodRegister")) 24
    else if (s.contains("dotProduct") && s.contains("prod") && s.contains(".i")) 16
    else if (s.contains("dotProduct") && s.contains("prod") && s.contains(".o")) 24
    else if (s.contains("dotProduct") && s.contains("add.i")) 24
    else if (s.contains("dotProduct") && s.contains("add.o")) 32
    else if (s.contains("dotProduct") && s.contains("addRegister")) 32
    else if (s.contains("dotProduct") && s.contains("max")) 32
    else if (s.contains("extractor") || s.contains(".o_") || s.contains(".i_")) 16
    else {
      //      println(s"[WARNING] $s does not belong to conv")
      0
    }

  def bitwidthPerNameInDense(s: String): Int =
    if (s.contains("outRegister")) 16
    else if (s.contains("prodRegister")) 24
    else if (s.contains("prod") && s.contains(".o")) 24
    else if (s.contains("prod") && s.contains(".i")) 16
    else if (s.contains("add.i_acc")) 32
    else if (s.contains("add.i")) 24
    else if (s.contains("add.o")) 32
    else if (s.contains("accRegister")) 32
    else if (s.contains("biasAdd")) 32
    else if (s.contains("outRegister")) 16
    else {
      //      println(s"[WARNING] $s does not belong to dense")
      0
    }

  def bitwidthPerNameInPooling(s: String): Int =
    if (s.contains("Extractor") || s.contains("Max")) 16
    else {
      //      println(s"[WARNING] $s does not belong to pooling")
      0
    }

  def bitwidthPerNameInSequencer(s: String): Int =
    if (s.contains(".r") || s.contains("mux")) 16
    else {
      //      println(s"[WARNING] $s does not belong to sequencer")
      0
    }


  def faultEquivalenceAnalysisLayer(l: Layer2D, inputFlow: Stream[Int]) = {
    val saxClasses = ((for {
      saxFile <- locateTempFile(s"sax-${l.name}")
    } yield {
      println("[INFO] Found previous run checkpoint, bypassing equivalence classes computing")
      val file = Source.fromFile(saxFile)
      val saxLocalEq = file.parse[UnionFind[String]]
      file.close()
      saxLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val saxUnionFind = l.stuckAtXClasses
      ("sax-" + l.name).export(saxUnionFind)
      saxUnionFind
    }).equivalenceClasses
    val bfClasses = (for {
      bfFile <- locateTempFile(s"bf-${l.name}")
    } yield {
      println("[INFO] Found previous run checkpoint, bypassing equivalence classes computing")
      val file = Source.fromFile(bfFile)
      val bfLocalEq = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      bfLocalEq
    }).getOrElse {
      println("[INFO] Found no previous run checkpoint, start computing equivalence classes")
      val bfUnionFind = l.bitFlipClasses(inputFlow)
      ("bf-" + l.name).export(bfUnionFind)
      bfUnionFind.equivalenceClasses
    }
    val saxCount = l.stuckAtCount
    val bfCount = l.bitFlipsCount(inputFlow)

    //("sax" + l.name).export(saxClasses.map(_.toStream).toStream)
  val (bfClassWidth,saxClassWidth) = l match {
    case _: Convolution2D => {
      (bfClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInConv).max)), saxClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInConv).max)))
    }
    case _: Pooling2D => {
      (bfClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInPooling).max)), saxClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInPooling).max)))
    }
    case _: Dense => {
      (bfClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInDense).max)), saxClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInDense).max)))
    }
    case _: Sequencer => {
      (bfClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInSequencer).max)), saxClasses.map(eqClass => (eqClass, eqClass.map(bitwidthPerNameInSequencer).max)))
    }
  }
    println(s"number of SAX=$saxCount \t number of bit-flips=$bfCount")
    //println(s"number of SAX classes=${saxClasses.size} \t number of bit-flips classes=${bfClasses.size}")
    println(s"number of SAX classes=${saxClassWidth.toSeq.map(_._2).sum} \t number of bit-flips classes=${bfClassWidth.toSeq.map(_._2).sum}")
    val saxClassPerSize = saxClassWidth.groupBy(_._1.size)
      .mapValues(m => m.toSeq.map(p => p._2).sum)
    println(s"SAX Classes per size:${saxClassPerSize}")
    val bfClassPerSize = bfClassWidth.groupBy(_._1.size)
      .mapValues(m => m.toSeq.map(p => p._2).sum)
    println(s"BF Classes per size:${bfClassPerSize}")
    //    println("Comparing to the LenetAnalysis")
    //    stuckAtLeNetAnalysis(LeNetFixture.LeNetFixtureSafecompPaper,saxClasses)
  }

  "the number of scenarios for a small convolution with discontinuous inputs" should "be consistent" in {
    val clu = Convolution2D("clu", Shape2D( 3, 3),Shape2D(1,1), Shape2D(2,2),valid_padding = true,1)
    faultEquivalenceAnalysisLayer(clu, inputSmall)
  }
  "the number of scenarios for a larger convolution" should "be countable with function" in {
    val clu = Convolution2D("clu", Shape2D(5, 5,2), Shape2D(1, 1), Shape2D(2, 2), valid_padding = true, 4)
    val inputFlow = (0 until 5 * 5).toStream
    val t = clu.transform(inputFlow)
    val saScenarios = clu.stuckAtScenarios
    val bfScenarios = clu.bitFlipsScenarios(inputFlow)
    faultEquivalenceAnalysisLayer(clu, inputFlow)
  }

"Fallible" should "identify all failure scenarios in a small pooling layer unit" in {
  val plu = Pooling2D("plu", Shape2D(4, 4, 2), Shape2D(2, 2), Shape2D(2, 2), valid_padding = true)
  val inputFlow = (0 until 4 * 4).toStream
  val t = plu.transform(inputFlow)
  val saScenarios = plu.stuckAtScenarios
  val bfScenarios = plu.bitFlipsScenarios(inputFlow)
  val bfClasses = plu.bitFlipClasses(inputFlow)
  faultEquivalenceAnalysisLayer(plu, inputFlow)
}
  "the number of scenarios for the convolution CONV1" should "be countable" in {
    val clu = Convolution2D("CONV1", Shape2D( 32, 32),Shape2D(1,1), Shape2D(5,5),valid_padding = true,6)
    faultEquivalenceAnalysisLayer(clu,inputLenet)

  }
  "the number of scenarios for the pooling POOL1" should "be countable" in {
    val plu = Pooling2D("POOL1", Shape2D( 28, 28,6),Shape2D(2,2), Shape2D(2,2),valid_padding = true)
    val inputFlow = (1 to 28 * 28).toStream

    //val bfClasses = plu.bitFlipClasses(inputFlow)
    //val errorClasses = bfClasses.equivalenceClasses.filter(_.exists(c => c.contains("horizon") || c.contains("r0") || c.contains("r28")))
    faultEquivalenceAnalysisLayer(plu,inputFlow)
  }

  "the number of scenarios for a small dense layer" should "be countable" in {
    val plu = Dense("FC", Shape2D(2, 2, 3), Shape2D(1, 1, 4))
    val inputFlow = (1 to 2 * 2).toStream
    val dlu = plu.transform(inputFlow)
    val saScenarios = plu.stuckAtScenarios
    val bfScenarios = plu.bitFlipsScenarios(inputFlow)
    faultEquivalenceAnalysisLayer(plu, inputFlow)
  }
  "the number of scenarios for the dense FC1" should "be countable" in {
    val plu = Dense("FC1", Shape2D(5, 5, 16), Shape2D(1,1, 120))
    val inputFlow = (1 to 5 * 5).toStream
    faultEquivalenceAnalysisLayer(plu,inputFlow)
  }


  "the number of scenarios for a small sequencer" should "be countable" in {
    val inputFlow = Stream(0)
    val plu = Sequencer("SEQ", Shape2D(1, 1, 5), Shape2D(5, 1, 1))
    val reginit = plu.transform(inputFlow).registers.zipWithIndex.init
    val bfCounts = plu.bitFlipsCount(inputFlow)
    val saxCount = plu.stuckAtCount
    val saxScenarios = plu.stuckAtScenarios
    val bfScenarios = plu.bitFlipsScenarios(inputFlow)
    val saxClasses = plu.stuckAtXClasses.equivalenceClasses
    val bfClasses = plu.bitFlipClasses(inputFlow).equivalenceClasses
    println(s"bf classes size: ${bfClasses.size}")
  }

  def stuckAtLeNetAnalysis(network: ConvolutionalNeuralNetwork, classes: Set[Set[String]]): Seq[(Int, (Int, Int))] = {
    //    val discard = unionFind.elements.filter(id => network.layers.exists(l => id.contains(s"${l.name}.i") || id.contains(s"${l.name}.o")))
    //    discard.foreach(unionFind.discard)
    val weightedClassesBySize = classes.toSeq
      .groupBy(s => s.size)
      .transform((_, classes) => {
        val SAX = classes.map(eqClass => eqClass.toSeq.map(f =>
          (for {l <- network.layers.find(x => f.contains(x.name))} yield {
            l match {
              case _: Convolution2D => bitwidthPerNameInConv(f)
              case _: Pooling2D => bitwidthPerNameInPooling(f)
              case _: Dense => bitwidthPerNameInDense(f)
              case _: Sequencer => bitwidthPerNameInSequencer(f)
              case _: Input2D => 0
            }
          }) getOrElse {
            println(s"$f not in any layer");
            0
          }))
        (classes, SAX.map(_.max).sum, SAX.map(_.sum).sum)
      })
      .toSeq
      .sortBy(_._1)
    val summarizedClasses = weightedClassesBySize.map(e => (e._1, (e._2._2, e._2._3)))
    println(summarizedClasses.map(kv => s"${kv._2._2} SA in equivalence classes of size ${kv._1} that can be covered with ${kv._2._1} injections")
      .mkString(s"Exhaustive SA: ${summarizedClasses.map(_._2._2).sum}\nmiminal SA to be injected: ${summarizedClasses.map(_._2._1).sum}\nDetails:\n\t", "\n\t", ""))

    summarizedClasses
  }

  def bitFlipLeNetAnalysis(network: ConvolutionalNeuralNetwork, classes: Set[Set[String]]): Seq[(Int, (Int, Int))] = {
    //    val discard = unionFind.elements.filter(id => network.layers.exists(l => id.contains(s"${l.name}.i") || id.contains(s"${l.name}.o")))
    //    discard.foreach(unionFind.discard)
    val summarizedClasses = classes.toSeq
      .groupBy(s => s.size)
      .transform((_, v) => {
        val BF = v.map(s => s.toSeq.map(f =>
          List(bitwidthPerNameInDense(f),
            bitwidthPerNameInConv(f),
            bitwidthPerNameInPooling(f),
            bitwidthPerNameInSequencer(f)).max))
        (BF.map(_.max).sum, BF.map(_.sum).sum)
      })
      .toSeq
      .sortBy(_._1)
    val groupedClasses = summarizedClasses.map({p =>
      p._1 match {
        case 1 => ("1", p)
        case i if (i >= 2 && i <= 30) => ("2 -> 30", p)
        case i if (i > 30 && i <= 60) => ("31 -> 60", p)
        case i if (i > 60 && i <= 90) => ("61 -> 90", p)
        case _ => ("91 -> xxx", p)
      }
    }).groupBy(_._1)
      val summarizeGroupedClasses = groupedClasses.map(e =>{
     if (e._1.contains("xxx")) (e._1.replaceAll("xxx",e._2.map(_._2._1).max.toString),e._2.map(_._2._2))
     else (e._1,e._2.map(_._2._2))
    }).mapValues(_.foldLeft((0,0)){(acc,e) => (acc._1+e._1,acc._2+e._2)})

    println(summarizedClasses.map(kv => s"${kv._2._2} BF in equivalence classes of size ${kv._1} that can be covered with ${kv._2._1} injections")
      .mkString(s"Exhaustive BF: ${summarizedClasses.map(_._2._2).sum}\nmiminal BF to be injected: ${summarizedClasses.map(_._2._1).sum}\nDetails:\n\t", "\n\t", ""))

    summarizedClasses
  }

  "the number of scenarios for the full LeNet" should "be countable" in {
    val plu = LeNetFixture.LeNetFixtureSafecompPaper
    val saxClasses = (for {
      saxFile <- locateTempFile(s"sax-${plu.name}")
    } yield {
      val file = Source.fromFile(saxFile)
      val classes = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      classes
    }).getOrElse {
      println("[INFO] found no checkpoint, generating classes")
      val classes = plu.stuckAtXClasses
      s"sax-${plu.name}".export(classes)
      classes.equivalenceClasses
    }

    val bfClasses = (for {
      bfFile <- locateTempFile(s"bf-${plu.name}")
    } yield {
      val file = Source.fromFile(bfFile)
      val classes = file.parse[LocalEquivalenceClasses].classes.map(_.toSet).toSet
      file.close()
      classes
    }).getOrElse {
      println("[INFO] found no checkpoint, generating classes")
      val classes = plu.stuckAtXClasses
      s"bf-${plu.name}".export(classes)
      classes.equivalenceClasses
    }
    stuckAtLeNetAnalysis(plu, saxClasses)
    bitFlipLeNetAnalysis(plu, bfClasses)
  }
  }
