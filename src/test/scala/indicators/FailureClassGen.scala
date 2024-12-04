package indicators

import org.scalacheck.Gen

object FailureClassGen{
  val failureClassGen: Gen[FailureClass] = for {
    ratio <- Gen.chooseNum(Float.MinValue,Float.MaxValue)
    depth <- Gen.chooseNum(Int.MinValue,Int.MaxValue)
    failureClass <- Gen.oneOf(XMisclassification(depth),Degraded(ratio),Masked,Observed)
  } yield failureClass


}
