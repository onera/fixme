package operators

import models.LeNetFixture.LeNetFixtureSafecompPaper._
import models.LeNetFixture.VhdlImplementedLeNetFixture
import models._
import operators.Fallible._
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.language.{higherKinds, implicitConversions, reflectiveCalls}

class InjectedTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {

  "The number of injected stuck at X in a convolution layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs,ws,hker,wker,hin,win) = (1,1,2,2,4,4)
    val (cin,cout) = (2,3)
    val result = Convolution2D("conv",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hker,wker),valid_padding = true,3)
    val expectedREG16 = cin
    val expectedREG24 = 0
    val expectedREG32 = 0
    val expectedProd = 0
    val expectedAdd = 0
    val expectedMax = 0
    val expectedOut = cout

    result.injectedStuckAt shouldBe expectedREG16 * 16 +
      expectedREG24 * 24 +
      expectedREG32 * 32 +
      expectedProd * stuckAtXComb(Array(16),24) +
      expectedAdd * 32 +
      expectedMax * 32 +
      expectedOut * 16
  }

  "The number of injected stuck at X in a convolution layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs,ws,hker,wker,hin,win) = (1,1,2,2,4,4)
    val (cin,cout) = (2,3)
    val result = Convolution2D("conv",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hker,wker),valid_padding = true,3)
    val expectedREG16 = cin
    val expectedREG24 = cin * cout * hker * wker
    val expectedREG32 = cout
    val expectedProd = 0
    val expectedAdd = 0
    val expectedMax = 0
    val expectedOut = cout

    result.injectedStuckAt shouldBe expectedREG16 * 16 +
      expectedREG24 * 24 +
      expectedREG32 * 32 +
      expectedProd * stuckAtXComb(Array(16),24) +
      expectedAdd * 32 +
      expectedMax * 32 +
      expectedOut * 16
  }

  "The number of injected stuck at X in a convolution layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs,ws,hker,wker,hin,win) = (1,1,2,2,4,4)
    val (cin,cout) = (2,3)
    val result = Convolution2D("conv",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hker,wker),valid_padding = true,3)
    val expectedREG16 = cin * hker * wker
    val expectedREG24 = cin * cout * hker * wker
    val expectedREG32 = cout
    val expectedProd = 0
    val expectedAdd = 0
    val expectedMax = 0
    val expectedOut = 0

    result.injectedStuckAt shouldBe expectedREG16 * 16 +
      expectedREG24 * 24 +
      expectedREG32 * 32 +
      expectedProd * stuckAtXComb(Array(16),24) +
      expectedAdd * 32 +
      expectedMax * 32 +
      expectedOut * 16
  }

  "The number of injected bitflips in a convolution layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs,ws,hker,wker,hin,win) = (1,1,2,2,4,4)
    val (cin,cout) = (2,3)
    val result = Convolution2D("conv",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hker,wker),valid_padding = true,3)
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cin * vulnerableCount(transformed.neighbourExtractor.registers.head.output.tFlow)
    val expectedREG24 = 0
    val expectedREG32 = 0
    val expectedOutput = 16 * cout * vulnerableCount(transformed.output.tFlow)
    result.injectedBitFlips(customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedOutput
  }

  "The number of injected bitflips in a convolution layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs,ws,hker,wker,hin,win) = (1,1,2,2,4,4)
    val (cin,cout) = (2,3)
    val result = Convolution2D("conv",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hker,wker),valid_padding = true,3)
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cin * vulnerableCount(transformed.neighbourExtractor.registers.head.output.tFlow)
    val expectedREG24 = 24 * cin * cout * hker * wker * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
    val expectedOutput = 16 * cout * vulnerableCount(transformed.output.tFlow)
    result.injectedBitFlips(customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedOutput
  }

  "The number of injected bitflips in a convolution layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs,ws,hker,wker,hin,win) = (1,1,2,2,4,4)
    val (cin,cout) = (2,3)
    val result = Convolution2D("conv",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hker,wker),valid_padding = true,3)
    val transformed = result.transform(customElement)
    val expectedREG16 = 16 * cin * transformed.neighbourExtractor
      .shiftRegisters.map(_.size - 1)
      .map(transformed.neighbourExtractor.registers)
      .map(_.output.tFlow)
      .map(vulnerableCount)
      .sum
    val expectedREG24 = 24 * cin * cout * hker * wker * vulnerableCount(transformed.convolutionChannelUnit.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * vulnerableCount(transformed.convolutionChannelUnit.addRegister.output.tFlow)
    val expectedOutput = 0
    result.injectedBitFlips(customInput) shouldBe expectedREG16 + expectedREG24 + expectedREG32 + expectedOutput
  }

  "The number of injected stuck at X in a pooling layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val cin = 2
    val result = Pooling2D("pooling",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hpool,wpool),valid_padding = true)
    val expectedREG16 = 2 * cin
    result.injectedStuckAt shouldBe expectedREG16 * 16
  }

  "The number of injected stuck at X in a pooling layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val cin = 2
    val result = Pooling2D("pooling",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hpool,wpool),valid_padding = true)
    val expectedREG16 = 3 * cin
    result.injectedStuckAt shouldBe expectedREG16 * 16
  }

  "The number of injected stuck at X in a pooling layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val cin = 2
    val result = Pooling2D("pooling",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hpool,wpool),valid_padding = true)
    val expectedREG16 = (hpool + wpool) * cin
    result.injectedStuckAt shouldBe expectedREG16 * 16
  }

  "The number of injected bitflips in a pooling layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val cin = 2
    val result = Pooling2D("pooling",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hpool,wpool),valid_padding = true)
    val transformed = result.transform(customElement)
    val expectedREG16Vertical = 16 * cin * vulnerableCount(transformed.verticalExtractor.registers.head.output.tFlow)
    val expectedREG16Horizontal = 0
    val outputInjected = 16 * cin * vulnerableCount(transformed.output.tFlow)
    result.injectedBitFlips(customInput) shouldBe expectedREG16Vertical + expectedREG16Horizontal + outputInjected
  }

  "The number of injected bitflips in a pooling layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val cin = 2
    val result = Pooling2D("pooling",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hpool,wpool),valid_padding = true)
    val transformed = result.transform(customElement)
    val expectedREG16Vertical = 16 * cin * vulnerableCount(transformed.verticalExtractor.registers.head.output.tFlow)
    val expectedREG16Horizontal = 16 * cin * vulnerableCount(transformed.horizontalExtractor.registers.head.output.tFlow)
    val outputInjected = 16 * cin * vulnerableCount(transformed.output.tFlow)
    result.injectedBitFlips(customInput) shouldBe expectedREG16Vertical + expectedREG16Horizontal + outputInjected
  }

  "The number of injected bitflips in a pooling layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hs,ws,hpool,wpool,hin,win) = (2,2,2,2,4,4)
    val cin = 2
    val result = Pooling2D("pooling",Shape2D(hin,win,cin),Shape2D(hs,ws),Shape2D(hpool,wpool),valid_padding = true)
    val transformed = result.transform(customElement)
    val verticalIndices = transformed.verticalExtractor.shiftRegisters.map(_.size - 1)
    val horizontalIndices = transformed.horizontalExtractor.shiftRegisters.map(_.size - 1)
    val expectedREG16Vertical = 16 * cin * verticalIndices.map(transformed.verticalExtractor.registers).map(_.output.tFlow).map(vulnerableCount).sum
    val expectedREG16Horizontal = 16 * cin * horizontalIndices.map(transformed.horizontalExtractor.registers).map(_.output.tFlow).map(vulnerableCount).sum
    result.injectedBitFlips(customInput) shouldBe  expectedREG16Vertical + expectedREG16Horizontal
  }

  "The number of injected stuck at X in a dense layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (cin,cout) = (120,30)
    val result = Dense("dense",Shape2D(4,4,cin),Shape2D(1,1,cout))
    val expectedREG16 = 0
    val expectedREG24 = 0
    val expectedREG32 =  0
    val inputInjected = cin * 16
    result.injectedStuckAt shouldBe expectedREG16 + expectedREG24 + expectedREG32 + inputInjected
  }

  "The number of injected stuck at X in a dense layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (cin,cout) = (120,30)
    val result = Dense("dense",Shape2D(4,4,cin),Shape2D(1,1,cout))
    val expectedREG16 = 0
    val expectedREG24 = 24 * cin * cout
    val expectedREG32 =  32 * cout
    val inputInjected = cin * 16
    result.injectedStuckAt shouldBe expectedREG16 + expectedREG24 + expectedREG32 + inputInjected
  }

  "The number of injected stuck at X in a dense layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (cin,cout) = (120,30)
    val result = Dense("dense",Shape2D(4,4,cin),Shape2D(1,1,cout))
    val expectedREG16 = 0
    val expectedREG24 = 24 * cin * cout
    val expectedREG32 =  32 * 2 * cout
    val inputInjected = 0
    result.injectedStuckAt shouldBe expectedREG16 + expectedREG24 + expectedREG32 + inputInjected
  }

  "The number of injected bitflips in a dense layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (cin,cout) = (120,30)
    val result = Dense("dense",Shape2D(4,4,cin),Shape2D(1,1,cout))
    val transformed = result.transform(customElement)
    val expectedREG16 = 0
    val expectedREG24 = 0
    val expectedREG32 =  0
    val injectedInput =  16 * cin  * vulnerableCount(transformed.input.tFlow)
    result.injectedBitFlips(customElement) shouldBe expectedREG16 + expectedREG24 + expectedREG32 + injectedInput
  }

  "The number of injected bitflips in a dense layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (cin,cout) = (120,30)
    val result = Dense("dense",Shape2D(4,4,cin),Shape2D(1,1,cout))
    val transformed = result.transform(customElement)
    val expectedREG16 = 0
    val expectedREG24 = 24 * cout * cin * vulnerableCount(transformed.prodRegister.output.tFlow)
    val expectedREG32 = 32 * cout * vulnerableCount(transformed.accRegister.resultOut.tFlow)
    val injectedInput =  16 * cin * vulnerableCount(transformed.input.tFlow)
    result.injectedBitFlips(customElement) shouldBe expectedREG16 + expectedREG24 + expectedREG32 + injectedInput
  }

  "The number of injected bitflips in a dense layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (cin,cout) = (120,30)
    val result = Dense("dense",Shape2D(4,4,cin),Shape2D(1,1,cout))
    val transformed = result.transform(customElement)
    val expectedREG16 = 0
    val expectedREG24 = 24 * cout * cin * vulnerableCount(transformed.prodRegister.output.tFlow)
    val expectedREG32 = 2 * 32 * cout * vulnerableCount(transformed.accRegister.resultOut.tFlow)
    val injectedInput = 0
    result.injectedBitFlips(customElement) shouldBe expectedREG16 + expectedREG24 + expectedREG32 + injectedInput
  }

  "The number of injected stuck at X in a sequencer layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hin,win) = (1,1)
    val (cin,cout) = (18,2)
    val result = Sequencer("sequencer",Shape2D(hin,win,cin),Shape2D(cout/cin,1,cout))
    val expectedREG16 = cout
    result.injectedStuckAt shouldBe expectedREG16 * 16
  }

  "The number of injected stuck at X in a sequencer layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hin,win) = (1,1)
    val (cin,cout) = (18,2)
    val result = Sequencer("sequencer",Shape2D(hin,win,cin),Shape2D(cout/cin,1,cout))
    val expectedREG16 = cout
    result.injectedStuckAt shouldBe expectedREG16 * 16
  }

  "The number of injected stuck at X in a sequencer layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hin,win) = (1,1)
    val (cin,cout) = (18,2)
    val result = Sequencer("sequencer",Shape2D(hin,win,cin),Shape2D(cout/cin,1,cout))
    val expectedREG16 =  cout
    result.injectedStuckAt shouldBe expectedREG16 * 16
  }

  "The number of injected bitflips in a sequencer layer by input injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.InputBased
    val (hin,win) = (1,1)
    val (cin,cout) = (18,2)
    val result = Sequencer("sequencer",Shape2D(hin,win,cin),Shape2D(cout/cin,1,cout))
    val transformed = result.transform(Stream(0))
    val expectedInt = cin * 16 * vulnerableCount(transformed.input.tFlow)
    val expectedOut = 0
    result.injectedBitFlips(Stream(0)) shouldBe expectedInt + expectedOut
  }

  "The number of injected bitflips in a sequencer layer by first register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.FirstRegisterBased
    val (hin,win) = (1,1)
    val (cin,cout) = (18,2)
    val result = Sequencer("sequencer",Shape2D(hin,win,cin),Shape2D(cout/cin,1,cout))
    val transformed = result.transform(Stream(0))
    val expectedInt = cin * 16 * vulnerableCount(transformed.input.tFlow)
    val expectedOut = 0
    result.injectedBitFlips(Stream(0)) shouldBe expectedInt + expectedOut
  }

  "The number of injected bitflips in a sequencer layer by out register injection" should "be consistent for the custom example" in {
    import models.InjectionStrategy.Implicits.OutputRegisterBased
    val (hin, win) = (1, 1)
    val (cin, cout) = (18, 2)
    val result = Sequencer("sequencer", Shape2D(hin, win, cin), Shape2D(cout / cin, 1, cout))
    val transformed = result.transform(Stream(0))
    val expectedRegisters = cout * 16 * vulnerableCount(transformed.registers.last.output.tFlow)
    result.injectedBitFlips(Stream(0)) shouldBe expectedRegisters
  }

  "The number of injected stuck-at in the first convolution layer by my custom injection strategy" should "be consistent for leNet5" in {
    import models.InjectionStrategy.Implicits.MyInjectionStrategy
    val (hin, win) = (32, 32)
    val (cin, cout) = (1, 6)
    val result = Convolution2D("conv2d", Shape2D(hin, win, cin), Shape2D(1, 1), Shape2D(5, 5), true, cout)
    val transformed = result.transform(Stream(0))
    val expected = cout * 16 + 16
    result.injectedStuckAt shouldBe expected
  }

  "The number of injected stuck-at in the total CNN by my custom injection strategy" should "be consistent for leNet5" in {
    import models.InjectionStrategy.Implicits.MyInjectionStrategy
    val result = VhdlImplementedLeNetFixture
    val saOverall = result.layers.map(_.injectedStuckAt)
    val expected = List(112, 192, 352, 512, 256, 160, 160, 336, 336)
    saOverall shouldBe expected
  }
}
