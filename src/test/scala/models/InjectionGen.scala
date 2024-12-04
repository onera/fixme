package models

import models.ConvolutionalNeuralNetworkGen.layer2DGen
import org.scalacheck.Gen
import utils.FaultTypeGen

object InjectionGen {

  /**
   * Define a way to generate random injection
   */
  val injectionGen: Gen[Injection] = for {
    channelIndex <- Gen.choose(1,10)
    bitIndex <- Gen.choose(0,32)
    faultType <- FaultTypeGen.faultTypeGen
    startDate <- Gen.choose(0,1000)
    endDate <- Gen.choose(startDate,2000)
    layer <- layer2DGen
  } yield
    Injection(layer,channelIndex,bitIndex,faultType,startDate,endDate)

}
