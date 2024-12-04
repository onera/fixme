package utils

trait LabelledData {
  val index: Int
  val label: String
}

case class Image(index: Int, label: String) extends LabelledData

object Dataset extends Enumeration {

  protected case class Name(fileName: String) extends super.Val {}

  type Dataset = Value

  import scala.language.implicitConversions

  implicit def valueToDataset(x: Value): Name = x.asInstanceOf[Name]

  val MNIST = Name("mnist.csv")
}

