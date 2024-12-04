package utils

import scala.math.exp

object Softmax {
  def softmax(rawScores: Map[String, Double]): Map[String, Double] = {
    val expScores = rawScores.map(p => (p._1, exp(p._2)))
    val sum = expScores.values.sum
    expScores.map(p => (p._1, p._2 / sum))
  }
}
