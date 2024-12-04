package operators

import models.LeNetFixture.LeNetFixtureSafecompPaper
import models.LeNetFixture.LeNetFixtureSafecompPaper._
import models._
import operators.Fallible._
import operators.Injectable._
import operators.Transformable.{NeighbourExtractor, TFlow}
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import utils.FileManager

import scala.language.implicitConversions

class InjectableTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  case class AnalysisResult(injectedStuckAt:Int,
                            injectableStuckAtX: Int,
                            stuckAtX: Int,
                            injectedBitFlips: Int,
                            injectableBitFlips: Int,
                            bitFlips: Int)

  private def LetNetInjection(implicit strategy: InjectionStrategy): Option[AnalysisResult] = {
    val infos: Seq[(Layer2D, TFlow[Int])] = Seq(
      LeNetFixtureSafecompPaper.conv1 -> LeNetFixtureSafecompPaper.imageInput,
      LeNetFixtureSafecompPaper.pool1 -> LeNetFixtureSafecompPaper.conv1Output,
      LeNetFixtureSafecompPaper.conv2 -> LeNetFixtureSafecompPaper.pool1Output,
      LeNetFixtureSafecompPaper.pool2 -> LeNetFixtureSafecompPaper.conv2Output,
      LeNetFixtureSafecompPaper.fc1 -> LeNetFixtureSafecompPaper.pool2Output,
      LeNetFixtureSafecompPaper.seq1 -> LeNetFixtureSafecompPaper.fc1Output,
      LeNetFixtureSafecompPaper.fc2 -> LeNetFixtureSafecompPaper.seq1Output,
      LeNetFixtureSafecompPaper.seq2 -> LeNetFixtureSafecompPaper.fc2Output,
      LeNetFixtureSafecompPaper.fc3 -> LeNetFixtureSafecompPaper.seq2Output)

    for {
      writer <- FileManager.getFileWriter("coverage", s"LetNetWith$strategy.txt")
    } yield {
      val results = for {(l, f) <- infos} yield {
        var startTime = System.currentTimeMillis()
        val injectableBitFlips = l.injectableBitFlips(f)
        val bitFlips = l.bitFlipsCount(f)
        val injectedBitFlips = l.injectedBitFlips(f)
        val bitFlipExecTime = (System.currentTimeMillis() - startTime).toInt
        val bitFlipCoverage = if(bitFlips > 0) 100 * injectableBitFlips / bitFlips else 0
        val bitFlipConciseness = if(injectedBitFlips > 0) injectableBitFlips / injectedBitFlips else 0
        writer.write(
          s"""For ${l.name} bitFlips:
             | injected = $injectedBitFlips
             | covered = $injectableBitFlips
             | considered = $bitFlips
             | coverage ${if (bitFlipCoverage == 0) "< 1" else s"= $bitFlipCoverage"}%
             | conciseness = $bitFlipConciseness
             | estimated duration = ${strategy.injectionTime * injectedBitFlips}s
             | assessment time = $bitFlipExecTime ms
             | """.stripMargin)

        startTime = System.currentTimeMillis()
        val injectableStuckAtX = l.injectableStuckAtX
        val stuckAtX = l.stuckAtCount
        val injectedStuckAt = l.injectedStuckAt
        val stuckAtExecTime = (System.currentTimeMillis() - startTime).toInt
        val stuckAtCoverage = if(stuckAtX > 0) 100 * injectableStuckAtX / stuckAtX else 0
        val stuckAtConciseness = if(injectedStuckAt > 0 ) injectableStuckAtX / injectedStuckAt else 0

        writer.write(
          s"""For ${l.name} stuckAt:
             | injected = $injectedStuckAt
             | covered = $injectableStuckAtX
             | considered = $stuckAtX
             | coverage ${if (stuckAtCoverage == 0) "< 1" else s"= $stuckAtCoverage"}%
             | conciseness = $stuckAtConciseness
             | estimated duration = ${strategy.injectionTime * injectedStuckAt}s
             | assessment time = $stuckAtExecTime ms
             |\n""".stripMargin)
        Array(injectedStuckAt, injectableStuckAtX, stuckAtX, stuckAtExecTime, injectedBitFlips, injectableBitFlips, bitFlips, bitFlipExecTime)
      }

      val Seq(conv1,pool1,conv2,pool2,fc1,seq1,fc2,seq2,fc3) = results

      writer.write(s"conv SX = ${ 100 * (conv1(1) + conv2(1)) / (conv1(2) + conv2(2))}\n")
      writer.write(s"conv execTime SX = ${conv1(3) + conv2(3)}ms\n")
      writer.write(s"conv BF = ${ 100 * (conv1(5) + conv2(5)) / (conv1(6) + conv2(6))}\n")
      writer.write(s"conv execTime BF = ${conv1(7) + conv2(7)}ms\n")
      writer.write(s"pool SX = ${ 100 * (pool1(1) + pool2(1)) / (pool1(2) + pool2(2))}\n")
      writer.write(s"pool execTime SX = ${pool1(3) + pool2(3)}ms\n")
      writer.write(s"pool BF = ${ 100 * (pool1(5) + pool2(5)) / (pool1(6) + pool2(6))}\n")
      writer.write(s"pool execTime BF = ${pool1(7) + pool2(7)}ms\n")
      writer.write(s"fc SX = ${ 100 * (fc1(1) + fc2(1)+ fc3(1)) / (fc1(2) + fc2(2) + fc3(2))}\n")
      writer.write(s"fc execTime SX = ${fc1(3) + fc2(3) + fc3(3)}ms\n")
      writer.write(s"fc BF = ${ 100 * (fc1(5) + fc2(5)+ fc3(5)) / (fc1(6) + fc2(6) + fc3(6))}\n")
      writer.write(s"fc execTime BF = ${fc1(7) + fc2(7) + fc3(7)}ms\n")
      writer.write(s"seq SX = ${ 100 * (seq1(1) + seq2(1)) / (seq1(2) + seq2(2))}\n")
      writer.write(s"seq execTime SX = ${seq1(3) + seq2(3)}ms\n")
      writer.write(s"seq BF = ${ 100 * (seq1(5) + seq2(5)) / (seq1(6) + seq2(6))}\n")
      writer.write(s"sec execTime BF = ${seq1(7) + seq2(7)}ms\n")


      val Array(injectedStuckAt, injectableStuckAtX, stuckAtX, stuckAtExecTime, injectedBitFlips, injectableBitFlips, bitFlips, bitFlipExecTime) =
        results.reduce((l, r) => l.zip(r).map(p => p._1 + p._2))
      val bitFlipCoverage = 100 * injectableBitFlips / bitFlips
      val stuckAtCoverage = 100 * injectableStuckAtX / stuckAtX
      //Remove double counted injection when performed on IO
      val (doubleCountedStuckAt, doubleCountedBitFlips) = (for {((l, _), (lp, fp)) <- infos.zip(infos.tail)
                                       if strategy.injectionPoints(l).contains(s"${l.name}.o") &&
                                         strategy.injectionPoints(lp).contains(s"${lp.name}.i")} yield {
          import models.InjectionStrategy.Implicits.InputOnly
          (lp.injectedStuckAt, lp.injectedBitFlips(fp))
      }).unzip
      val realInjectedStuckAt = injectedStuckAt - doubleCountedStuckAt.sum
      val realInjectedBitFlip = injectedBitFlips - doubleCountedBitFlips.sum
      writer.write(
        s"""For ${LeNetFixtureSafecompPaper.name} bitFlips:
           | injected = $realInjectedBitFlip
           | covered =  $injectableBitFlips
           | considered = $bitFlips
           | coverage ${if (bitFlipCoverage == 0) "< 1" else s"= $bitFlipCoverage"}%
           | conciseness = ${injectableBitFlips / realInjectedBitFlip}
           | estimated duration = ${strategy.injectionTime * realInjectedBitFlip}s
           | assessment time = $bitFlipExecTime ms
           | """.stripMargin)
      writer.write(
        s"""For ${LeNetFixtureSafecompPaper.name} stuckAt:
           | injected = $realInjectedStuckAt
           | covered = $injectableStuckAtX
           | considered = $stuckAtX
           | coverage ${if (stuckAtCoverage == 0) "< 1" else s"= $stuckAtCoverage"}%
           | conciseness = ${injectableStuckAtX / realInjectedStuckAt}
           | estimated duration = ${strategy.injectionTime * realInjectedStuckAt}s
           | assessment time = $stuckAtExecTime ms
           | """.stripMargin)
      writer.flush()
      writer.close()
      AnalysisResult(realInjectedStuckAt,injectableStuckAtX,stuckAtX,realInjectedBitFlip,injectableBitFlips,bitFlips)
    }
  }

  "The injectable point in extractor" should "be dates when the data is always used corrupted" in {
    val (hs, ws, hp, wp, hker, wker, hin, win) = (1, 1, 0, 0, 2, 2, 4, 4)
    val wout = (win + 2 * wp - wker) / ws + 1
    val hout = (hin + 2 * hp - hker) / hs + 1
    wout shouldBe 3
    hout shouldBe 3
    val size = (hker - 1) * win + wker
    size shouldBe 6
    val extractor = NeighbourExtractor(s"extractor", customElement, hs, ws, hp, wp, hker, wker, hin, win)
    val extractorCorr = NeighbourExtractor(s"extractor", customCorruptibleElement, hs, ws, hp, wp, hker, wker, hin, win)
    val brutForceSimu = injectableExhaustive(extractorCorr)
    val optimizedSimu = injectable(extractor)
    val outputInjectable = injectableOutput(extractor)
    val injectableInRegister = injectableWithRegisters(extractor, Array(0))
    val injectableOutRegisters = injectableWithRegisters(extractor, extractor.shiftRegisters.map(_.size - 1))
    for {i <- optimizedSimu.indices} {
      optimizedSimu(i) shouldBe brutForceSimu(i).map(e => Element(e.date, e.dataId))
      optimizedSimu(i) shouldBe injectableInRegister(i)
    }

    optimizedSimu(0) shouldBe Stream(0 -> 4, 1 -> 5, 2 -> 6, 3 -> 9, 4 -> 10, 5 -> 13, 6 -> 14, 7 -> 15, 8 -> 17, 9 -> 19, 10 -> 20, 11 -> 23, 12 -> 24, 13 -> 25, 14 -> 26, 15 -> 27).map(p => Element(p._2, p._1))
    optimizedSimu(1) shouldBe Stream(0 -> 5, 1 -> 6, 2 -> 9, 3 -> 10, 4 -> 13, 8 -> 19, 12 -> 25).map(p => Element(p._2, p._1))
    optimizedSimu(2) shouldBe Stream(0 -> 6, 1 -> 9, 2 -> 10, 3 -> 13).map(p => Element(p._2, p._1))
    optimizedSimu(3) shouldBe Stream(0 -> 9, 1 -> 10, 2 -> 13, 3 -> 14).map(p => Element(p._2, p._1))
    optimizedSimu(4) shouldBe Stream(0 -> 10, 1 -> 13, 2 -> 14, 3 -> 15).map(p => Element(p._2, p._1))
    optimizedSimu(5) shouldBe Stream(0 -> 13).map(p => Element(p._2, p._1))

    val coverage = (optimizedSimu.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    println(s"coverage on simple extractor by input bitFlip $coverage%")
    println(optimizedSimu.indices.map(i => optimizedSimu(i).mkString(s"injectable in r$i: ", ",", "")).mkString("\n"))
    val extendedCoverage = (outputInjectable.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    println(s"coverage on simple extractor by output bitFlip $extendedCoverage%")
    println(outputInjectable.indices.map(i => outputInjectable(i).mkString(s"injectable in r$i: ", ",", "")).mkString("\n"))
    val combinedCoverage = coverage + outputInjectable.indices.map(i => outputInjectable(i).filterNot(optimizedSimu(i).contains)).map(_.size).sum * 100 / extractor.registers.map(_.output.tFlow.size).sum
    println(s"total coverage on simple extractor by output & input bitFlip $combinedCoverage%")
    val coverageAnyRegister = (injectableOutRegisters.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    println(s"coverage on simple extractor by output register bitFlip $coverageAnyRegister%")
    println(injectableOutRegisters.indices.map(i => injectableOutRegisters(i).mkString(s"injectable in r$i: ", ",", "")).mkString("\n"))
  }

  it should "be dates when the data is always used corrupted for CONV1 neighbour extractor" in {
    val (hs, ws, hp, wp, hker, wker, hin, win) = (1, 1, 0, 0, 5, 5, 32, 32)
    val extractor = NeighbourExtractor(s"extractor", imageInputElement, hs, ws, hp, wp, hker, wker, hin, win)
    val result = injectable(extractor)
    val resultAny = injectableWithRegisters(extractor, extractor.shiftRegisters.map(_.size - 1))
    val injectableInRegister = injectableWithRegisters(extractor, Array(0))
    val coverage = (result.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    val outputInjectable = injectableOutput(extractor)
    for {i <- result.indices} {
      result(i) shouldBe injectableInRegister(i)
    }
    println(s"coverage on CONV1 extractor $coverage")
    println(result.indices.map(i => result(i).mkString(s"injectable in r$i: ", ",", "")).mkString("\n"))
    val combinedCoverage = coverage + outputInjectable.indices.map(i => outputInjectable(i).filterNot(result(i).contains)).map(_.size).sum * 100 / extractor.registers.map(_.output.tFlow.size).sum
    println(s"total coverage on simple extractor by output & input bitFlip $combinedCoverage%")
    val coverageAny = (resultAny.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    println(s"total coverage on simple extractor by output registers bitFlip $coverageAny%")
  }

  it should "be dates when the data is always used corrupted for CONV2 neighbour extractor" in {
    val (hs, ws, hp, wp, hker, wker, hin, win) = (1, 1, 0, 0, 5, 5, 14, 14)
    val extractor = NeighbourExtractor(s"extractor", pool1Output.map(i => Element(i, i)), hs, ws, hp, wp, hker, wker, hin, win)
    val result = injectable(extractor)
    val resultAny = injectableWithRegisters(extractor, extractor.shiftRegisters.map(_.size - 1))
    val injectableInRegister = injectableWithRegisters(extractor, Array(0))
    val coverage = (result.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    val outputInjectable = injectableOutput(extractor)
    for {i <- result.indices} {
      result(i) shouldBe injectableInRegister(i)
    }
    println(s"coverage on CONV2 extractor $coverage")
    println(result.indices.map(i => result(i).mkString(s"injectable in r$i: ", ",", "")).mkString("\n"))
    val combinedCoverage = coverage + outputInjectable.indices.map(i => outputInjectable(i).filterNot(result(i).contains)).map(_.size).sum * 100 / extractor.registers.map(_.output.tFlow.size).sum
    println(s"total coverage on simple extractor by output & input bitFlip $combinedCoverage%")
    val coverageAny = (resultAny.map(_.size).sum * 100) / extractor.registers.map(_.output.tFlow.size).sum
    println(s"total coverage on simple extractor by output registers bitFlip $coverageAny%")
  }

  "The number of covered stuck at X in a convolution layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val expectedREG16 = cin
    val expectedREG24 = 0
    val expectedREG32 = cout
    val expectedProd = 0
    val expectedAdd = cout
    val expectedMax = cout

    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16) +
      expectedREG24 * stuckAtXRegister(24) +
      expectedREG32 * stuckAtXRegister(31) +
      expectedProd * stuckAtXComb(Array(16), 24) +
      expectedAdd * 31 +
      expectedMax * (32 + 31)
  }

  "The number of covered stuck at X in a convolution layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val expectedREG16 = cin
    val expectedREG24 = cin * hker * wker * cout
    val expectedREG32 = cout
    val expectedProd = cin * hker * wker * cout
    val expectedAdd = cout
    val expectedMax = cout

    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16) +
      expectedREG24 * stuckAtXRegister(24) +
      expectedREG32 * stuckAtXRegister(32) +
      expectedProd * 24 +
      expectedAdd * stuckAtXComb(cin * hker * wker, 24, 32) +
      expectedMax * stuckAtXComb(1, 32)
  }

  "The number of covered stuck at X in a convolution layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val expectedREG16 = cin * hker * wker
    val expectedREG24 = cin * hker * wker * cout
    val expectedREG32 = cout
    val expectedProd = cin * hker * wker * cout
    val expectedAdd = cout
    val expectedMax = cout

    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16) +
      expectedREG24 * stuckAtXRegister(24) +
      expectedREG32 * stuckAtXRegister(32) +
      expectedProd * 24 +
      expectedAdd * stuckAtXComb(cin * hker * wker, 24, 32) +
      expectedMax * (31 + 32) //Cannot modify output bit without output injection
  }

  "The number of covered bitflips in a convolution layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cin * injectableWithRegisters(transformed.neighbourExtractor, Array(0)).map(vulnerableCount).sum
    val expectedREG24 = 0
    val expectedREG32 = 31 * cout * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
    result.injectableBitFlips(customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32
  }

  "The number of covered bitflips in a convolution layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cin * injectableWithRegisters(transformed.neighbourExtractor, Array(0)).map(vulnerableCount).sum
    val expectedREG24 = 24 * cin * hker * wker * cout * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
    result.injectableBitFlips(customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32
  }

  "The number of covered bitflips in a convolution layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs, ws, hker, wker, hin, win) = (1, 1, 2, 2, 4, 4)
    val (cin, cout) = (2, 3)
    val result = Convolution2D("conv", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hker, wker), valid_padding = true, 3)
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cin * injectableWithRegisters(transformed.neighbourExtractor,
      transformed.neighbourExtractor.shiftRegisters.map(_.size - 1)).map(vulnerableCount).sum
    val expectedREG24 = 24 * cin * hker * wker * cout * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
    result.injectableBitFlips(customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32
  }

  "The number of covered stuck at X in a pooling layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val expectedREG16 = cin
    val expectedMax = cin
    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16) + expectedMax * 16
  }

  "The number of covered stuck at X in a pooling layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val expectedREG16 = 2 * cin
    val expectedMax = 2 * cin
    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16) + expectedMax * 16
  }

  "The number of covered stuck at X in a pooling layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val expectedREG16 = (hpool + wpool) * cin
    val expectedMax = cin //no output injection
    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16) + expectedMax * 16
  }

  "The number of covered bitflips in a pooling layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val transformed = result.transform(customElement)
    val expectedREG16Vertical = 16 * cin * injectableWithRegisters(transformed.verticalExtractor, Array(0)).map(vulnerableCount).sum
    val expectedREG16Horizontal = 0
    result.injectableBitFlips(customInput) shouldBe expectedREG16Vertical + expectedREG16Horizontal
  }

  "The number of covered bitflips in a pooling layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val transformed = result.transform(customElement)
    val expectedREG16Vertical = 16 * cin * injectableWithRegisters(transformed.verticalExtractor, Array(0)).map(vulnerableCount).sum
    val expectedREG16Horizontal = 16 * cin * injectableWithRegisters(transformed.horizontalExtractor, Array(0)).map(vulnerableCount).sum
    result.injectableBitFlips(customInput) shouldBe expectedREG16Vertical + expectedREG16Horizontal
  }

  "The number of covered bitflips in a pooling layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs, ws, hpool, wpool, hin, win) = (2, 2, 2, 2, 4, 4)
    val cin = 2
    val result = Pooling2D("pooling", Shape2D(hin, win, cin), Shape2D(hs, ws), Shape2D(hpool, wpool), valid_padding = true)
    val transformed = result.transform(customElement)
    val verticalIndices = transformed.verticalExtractor.shiftRegisters.map(_.size - 1)
    val horizontalIndices = transformed.horizontalExtractor.shiftRegisters.map(_.size - 1)
    val expectedREG16Vertical = 16 * cin * injectableWithRegisters(transformed.verticalExtractor, verticalIndices).map(vulnerableCount).sum
    val expectedREG16Horizontal = 16 * cin * injectableWithRegisters(transformed.horizontalExtractor, horizontalIndices).map(vulnerableCount).sum
    result.injectableBitFlips(customInput) shouldBe expectedREG16Vertical + expectedREG16Horizontal
  }

  "The number of covered stuck at X in a dense layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val expectedREG16 = 0
    val expectedREG24 = 0
    val expectedREG32 = 0
    val expectedProd = 0
    val expectedAdd = 0
    result.injectableStuckAtX shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedAdd + expectedProd
  }

  "The number of covered stuck at X in a dense layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val expectedREG16 = 0 //cout * stuckAtXRegister(16)
    val expectedREG24 = cin * cout * stuckAtXRegister(24)
    val expectedREG32 = cout * stuckAtXRegister(32)
    val expectedProd = cout * cin * 24
    val expectedAdd = cout * (stuckAtXComb(Array.fill(cin)(24) :+ 32, 32) + stuckAtXComb(1, 32))
    result.injectableStuckAtX shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedAdd + expectedProd
  }

  "The number of covered stuck at X in a dense layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val expectedREG16 = cout * stuckAtXRegister(16)
    val expectedREG24 = cin * cout * stuckAtXRegister(24)
    val expectedREG32 = cout * stuckAtXRegister(32)
    val expectedProd = cout * cin * 24
    val expectedAdd = cout * (stuckAtXComb(Array.fill(cin)(24) :+ 32, 32) + stuckAtXComb(1, 32))
    result.injectableStuckAtX shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedAdd + expectedProd
  }

  "The number of covered bitflips in a dense layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val expectedREG16 = 0
    val expectedREG24 = 0
    val expectedREG32 = 0
    result.injectableBitFlips(customElement) shouldBe expectedREG16 + expectedREG24 + expectedREG32
  }

  "The number of covered bitflips in a dense layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val transformed = result.transform(customElement)
    val expectedREG16 = 0
    val expectedREG24 = 24 * cout * cin * vulnerableCount(transformed.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * transformed.accRegister.outputs.map(o => vulnerableCount(o.tFlow)).sum
    result.injectableBitFlips(customElement) shouldBe expectedREG16 + expectedREG24 + expectedREG32
  }

  "The number of covered bitflips in a dense layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (cin, cout) = (120, 30)
    val result = Dense("dense", Shape2D(4, 4, cin), Shape2D(1, 1, cout))
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cout * vulnerableCount(transformed.outRegister.output.tFlow)
    val expectedREG24 = 24 * cout * cin * vulnerableCount(transformed.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * transformed.accRegister.outputs.map(o => vulnerableCount(o.tFlow)).sum
    result.injectableBitFlips(customElement) shouldBe expectedREG16 + expectedREG24 + expectedREG32
  }

  "The number of covered stuck at X in a sequencer layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val expectedREG16 = cout //2 * cout
    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16)
  }

  "The number of covered stuck at X in a sequencer layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val expectedREG16 = cout // 2 * cout
    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16)
  }

  "The number of covered stuck at X in a sequencer layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val expectedREG16 = cout
    result.injectableStuckAtX shouldBe expectedREG16 * stuckAtXRegister(16)
  }

  "The number of covered bitflips in a sequencer layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val transformed = result.transform(Stream(0))
    val expectedREG16 = cout * 16 * transformed.registers.map(_.output.tFlow).map(vulnerableCount).sum
    result.injectableBitFlips(Stream(0)) shouldBe expectedREG16
  }

  "The number of covered bitflips in a sequencer layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val transformed = result.transform(Stream(0))
    val expectedREG16 = cout * 16 * transformed.registers.map(_.output.tFlow).map(vulnerableCount).sum
    result.injectableBitFlips(Stream(0)) shouldBe expectedREG16
  }

  "The number of covered bitflips in a sequencer layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val transformed = result.transform(Stream(0))
    val expectedREG16 = cout * 16 * transformed.registers.map(_.output.tFlow).map(vulnerableCount).sum
    result.injectableBitFlips(Stream(0)) shouldBe expectedREG16
  }

  "The injectable point in LetNet with inter layer injection strategy" should "provide the expected coverage" in {
    import models.InjectionStrategy.Implicits.InputBased
    for{ r <- LetNetInjection} yield {
      r.injectedStuckAt shouldBe 1200
      r.injectedBitFlips shouldBe 129344
    }
  }

  "The injectable point in LetNet with first layer injection strategy" should "provide the expected coverage" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    LetNetInjection
  }

  "The injectable point in LetNet with output register injection strategy" should "provide the expected coverage" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    LetNetInjection
  }
}
