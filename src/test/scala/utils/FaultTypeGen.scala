package utils

import org.scalacheck.Gen
import utils.FaultType._

object FaultTypeGen {
  /**
   * Define a way to generate a random failure mode
   */
  val faultTypeGen: Gen[FaultType] = Gen.frequency(
    1 -> NONE,
    1 -> BITFLIP,
    1 -> STUCK_AT_1,
    2 -> STUCK_AT_0)

}
