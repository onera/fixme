package models

import utils.FaultType.FaultType

case class Injection(layerId: Layer2D, channelIndex: Int, bitIndex: Int, faultType: FaultType, startDate: Int, stopDate: Int) {
  override def toString: String = s"${layerId.name}.${channelIndex}.${bitIndex}:${faultType}@${startDate}"
}
