package utils


object FaultType extends Enumeration {

  protected case class Val(directoryName: String) extends super.Val {}

  type FaultType = Value

  import scala.language.implicitConversions

  implicit def valueToFaultType(x: Value): Val = x.asInstanceOf[Val]

  val STUCK_AT_0 = Val("STUCK_AT_0")
  val STUCK_AT_1 = Val("STUCK_AT_1")
  val BITFLIP = Val("BITFLIP")
  val NONE = Val("NONE")
}
