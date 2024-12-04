package exporters

import exporters.FaultImpactExporter.restoreCheckpoint
import indicators.{Degraded, Masked, Observed, XMisclassification}
import models.LeNetFixture.{LeNetFixtureSafecompPaper, mkLeNetInputTFlow}
import models.{Convolution2D, Dense, Injection, Pooling2D}
import operators.all._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import parsers.MeasureResultsParser
import parsers.Parser.{parseCampaign, retrieveTimeSpecs}
import utils.Dataset.MNIST
import utils.FaultType.{BITFLIP, STUCK_AT_0, STUCK_AT_1}
import utils.FileManager.{extractResourceAsFile, getCampaignFiles}
import utils.UnionFind

import java.io.File

class ZeroImpactFaultExporterTest extends AnyFlatSpec with should.Matchers {
  it should "get only the fault locations with a SC-1 rate of 0" in {
    val res = MeasureResultsParser.parse(new File("output/measures/SC-1_by_fault_location.csv"))
      .drop("total", "rate")
      .collect()
      .map(row => (row.getString(0), row.getInt(1)))
      .filter(_._2 == 0)
    val regExp = """SC-1_fm:[A-Z0-9_]*_(\S*)""".r
    val uniqueRes = res.groupBy(_._1 match {
      case regExp(s) => s
      case p => p
    })
  }

  def convertInjectionToString(injection: Injection): Option[String] = {
    def toInt(c: String) = if (c.nonEmpty) c.toInt; else 0;
    val regExp = """(?:conv2d|maxpooling2d|dense)([0-9]*)""".r
    (for {
      ip <- injection.layerId match {
        case l: Dense => (l.name.filterNot(_ == '_') match {
          case regExp(c) => if (c == "")
            Some(s"FC1.i_")
          else Some(s"FC${toInt(c) + 1}.i_")
          case _ => None
        })
        case l: Convolution2D => (l.name.filterNot(_ == '_') match {
          case regExp(c) => if (c == "") Some(s"CONV1.i_") else Some(s"CONV${toInt(c) + 1}.i_")
          case _ => None
        })
        case l: Pooling2D => (l.name.filterNot(_ == '_') match {
          case regExp(c) => if (c == "") Some(s"POOL1.i_") else Some(s"POOL${toInt(c) + 1}.i_")
          case _ => None
        })
        case _ => None
      }
    } yield {
      ip + s"${injection.channelIndex}" + (if (injection.faultType == BITFLIP) {
        s"@${injection.startDate}"
      } else "") + s"+b${injection.bitIndex}"
    })

  }

  it should "get fault impact measures and compute the equivalence classes for Stuck at" in {
    for {
      path <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(path), MNIST)
      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
      data <- restoreCheckpoint(model, false, "faultImpactSAMerged")
    } yield {
      val dataByType = data.mapValues(_.filterNot(_._2 == 0)).groupBy(_._1.faultType)

      val totalInjections = dataByType.mapValues(_.size)
      val alwaysMaskedInjections = dataByType.mapValues(
        _.filter(e => e._2.forall(f => f._1 === Masked))
        //   .mapValues(_.values.sum)
      )
      val observedInjections = dataByType.mapValues(
        _.filter(e => e._2.forall(f => f._1 match {
          case Masked => true
          case Observed => true
          case Degraded(_) => true
          case _ => false
        })).filter(l => l._2.exists(p => p._1 match {
          case Observed => true
          case Degraded(_) => true
          case _ => false
        }))
        //    .mapValues(_.values.sum)
      )
      val atLeast1SCInjections = dataByType.mapValues(
        _.filter(e => e._2.exists(f => f._1 match {
          case XMisclassification(_) => true
          case _ => false
        }))
        // .mapValues(_.values.sum)
      )

      val onlySCInjections = dataByType.mapValues(
        _.filter(e => e._2.forall(f => f._1 match {
          case XMisclassification(_) => true
          case _ => false
        }))
        //  .mapValues(_.values.sum)
      )
      println(s"Total number of injections: ${totalInjections(STUCK_AT_0)} (SA-0), ${totalInjections(STUCK_AT_1)} (SA-1)")
      println(s"Number of injections always masked: ${alwaysMaskedInjections(STUCK_AT_0).size}(SA-0), ${alwaysMaskedInjections(STUCK_AT_1).size} (SA-1)")
      println(s"Number of injections without misclassification but with erroneous score: ${observedInjections(STUCK_AT_0).size} (SA-0), ${observedInjections(STUCK_AT_1).size} (SA-1)")
      println(s"Number of injections with at least 1 misclassification: ${atLeast1SCInjections(STUCK_AT_0).size} (SA-0), ${atLeast1SCInjections(STUCK_AT_1).size} (SA-1)")
      println(s"Number of injections with only misclassifications: ${onlySCInjections(STUCK_AT_0).size} (SA-0), ${atLeast1SCInjections(STUCK_AT_1).size} (SA-1)")

      val masked = alwaysMaskedInjections.mapValues(_.map(e => (convertInjectionToString(e._1), e._2)))
        .mapValues(_.keys.map(_.get)
          .groupBy(_.split('+').head)
          .mapValues(_.size))

      val observed = observedInjections.mapValues(_.map(e => (convertInjectionToString(e._1), e._2)))
        .mapValues(_.keys.map(_.get)
          .groupBy(_.split('+').head)
          .mapValues(_.size))

      val sc = atLeast1SCInjections.mapValues(_.map(e => (convertInjectionToString(e._1), e._2)))
        .mapValues(_.keys.map(_.get)
          .groupBy(_.split('+').head)
          .mapValues(_.size))
      val unionFind = LeNetFixtureSafecompPaper.stuckAtXClasses
      val classesMasked = masked.mapValues(getEquivalentClasses(_, unionFind))
      println(s"Number of SA-0 failure scenarios always masked = ${classesMasked(STUCK_AT_0)._1}")
      println(s"Number of SA-0 equivalent classes always masked = ${classesMasked(STUCK_AT_0)._2.values.sum}")
      println(s"${classesMasked(STUCK_AT_0)._2}")

      println(s"Number of SA-1 failure scenarios always masked = ${classesMasked(STUCK_AT_1)._1}")
      println(s"Number of SA-1 equivalent classes always masked = ${classesMasked(STUCK_AT_1)._2.values.sum}")
      println(s"${classesMasked(STUCK_AT_1)._2}")

      val observedClasses = observed.mapValues(getEquivalentClasses(_, unionFind))
      println(s"Number of SA-0 failure scenarios observed = ${observedClasses(STUCK_AT_0)._1}")
      println(s"Number of SA-0 equivalent classes observed = ${observedClasses(STUCK_AT_0)._2.values.sum}")
      println(s"${observedClasses(STUCK_AT_0)._2}")
      println(s"Number of SA-1 failure scenarios observed = ${observedClasses(STUCK_AT_1)._1}")
      println(s"Number of SA-1 equivalent classes observed = ${observedClasses(STUCK_AT_1)._2.values.sum}")
      println(s"${observedClasses(STUCK_AT_1)._2}")

      val scClasses = sc.mapValues(getEquivalentClasses(_, unionFind))
      println(s"Number of SA-0 failure scenarios with SC = ${scClasses(STUCK_AT_0)._1}")
      println(s"Number of SA-0 equivalent classes with SC = ${scClasses(STUCK_AT_0)._2.values.sum}")
      println(s"${scClasses(STUCK_AT_0)._2}")
      println(s"Number of SA-1 failure scenarios with SC = ${scClasses(STUCK_AT_1)._1}")
      println(s"Number of SA-1 equivalent classes with SC = ${scClasses(STUCK_AT_1)._2.values.sum}")
      println(s"${scClasses(STUCK_AT_1)._2}")
    }
  }

  def getEquivalentClasses(injections: Map[String, Int], unionFind: UnionFind[String]) = {

    val classes = unionFind
      .equivalenceClasses
      .filter(s => injections.exists(id => s.contains(id._1.split('+').head)))
      .map(p =>
        (p, {
          injections.find(e => p.contains(e._1.split('+').head)) match {
            case Some(str) => Some(injections(str._1))
            case None => None
          }
        })
      )
    val filteredClasses = classes
      .map(p =>
        (p._1.filterNot(id =>
          LeNetFixtureSafecompPaper.layers.exists(l =>
            id.contains(s"${l.name}.i") || id.contains(s"${l.name}.o")
          )
        )
          , p._2
        )
      )
    val countFM = filteredClasses.toList.map(p => p._1.size * p._2.getOrElse(0)).sum
    val countClasses = filteredClasses.toList.map(p => p._1.size -> p._2).groupBy(_._1).mapValues(_.map(_._2.get).sum)
    (countFM, countClasses)
  }

  it should "get fault impact measures and compute the equivalence classes for Bitflip" in {
    for {
      path <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(path), MNIST, exclude = Some(Seq("stuck")))
      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D)
      campaignOthers = parseCampaign(new File("data/leNet5/results/bitflips/campaign.conf"), physicalModel)
      campaignInput = parseCampaign(new File("data/leNet5/results/bitflips_inputLayer/campaign.conf"), physicalModel)
      campaign = campaignInput ++ campaignOthers.map(p => (p._1 + campaignInput.size, p._2))
      input <- model.getLayerByName("conv2d")
      conv1 <- model.getLayerByName("max_pooling2d")
      pool1 <- model.getLayerByName("conv2d_1")
      conv2 <- model.getLayerByName("max_pooling2d_1")
      pool2 <- model.getLayerByName("dense")
      dense1 <- model.getLayerByName("dense_1")
      dense2 <- model.getLayerByName("dense_2")
      data <- restoreCheckpoint(model, false, "faultImpactBFMerged")
    } yield {
      val timeSpecs = retrieveTimeSpecs(model, campaign)
      timeSpecs.size shouldBe 7
      val rawDataByType = data
        .mapValues(_.filterNot(_._2 == 0)) // filter out the failure classes without impact
        .groupBy(_._1.faultType)
        .mapValues(_.filterNot(_._1.layerId.name.matches("dense"))) //filter out the bitflips incoherent with architecture (no registers)
        .mapValues(_.filterNot(q => q._1.layerId == input && q._1.startDate > 1024))
      val network = LeNetFixtureSafecompPaper
      val dictionary = Map(
        input -> timeSpecs(input).filterNot(_ > 1024).sorted.zip(network.imageInput.toList.sorted).toMap,
        conv1 -> timeSpecs(conv1).sorted.zip(network.conv1Output.toList.sorted).toMap,
        pool1 -> timeSpecs(pool1).sorted.zip(network.pool1Output.toList.sorted).toMap,
        conv2 -> timeSpecs(conv2).sorted.zip(network.conv2Output.toList.sorted).toMap,
        pool2 -> timeSpecs(pool2).sorted.zip(network.pool1Output.toList.sorted).toMap,
        dense1 -> timeSpecs(dense1).sorted.zip(network.seq1Output.toList.sorted).toMap,
        dense2 -> timeSpecs(dense2).sorted.zip(network.seq2Output.toList.sorted).toMap
      )
      val dataByType = rawDataByType.filter(_._1 == BITFLIP).mapValues(
        p => p.map(m => (m._1.copy(startDate = dictionary(m._1.layerId)(m._1.startDate)), m._2)))

      val totalInjections = dataByType.mapValues(_.size)
      val alwaysMaskedInjections = dataByType.mapValues(
        _.filter(e => e._2.forall(f => f._1 === Masked))
        //   .mapValues(_.values.sum)
      )
      val observedInjections = dataByType.mapValues(
        _.filter(e => e._2.forall(f => f._1 match {
          case Masked => true
          case Observed => true
          case Degraded(_) => true
          case _ => false
        })).filter(l => l._2.exists(p => p._1 match {
          case Observed => true
          case Degraded(_) => true
          case _ => false
        }))
        //    .mapValues(_.values.sum)
      )
      val atLeast1SCInjections = dataByType.mapValues(
        _.filter(e => e._2.exists(f => f._1 match {
          case XMisclassification(_) => true
          case _ => false
        }))
        // .mapValues(_.values.sum)
      )

      val onlySCInjections = dataByType.mapValues(
        _.filter(e => e._2.forall(f => f._1 match {
          case XMisclassification(_) => true
          case _ => false
        }))
        //  .mapValues(_.values.sum)
      )
      println(s"Total number of injections: ${totalInjections(BITFLIP)}")
      println(s"Number of injections always masked: ${alwaysMaskedInjections(BITFLIP).size}")
      println(s"Number of injections without misclassification but with erroneous score: ${observedInjections(BITFLIP).size}")
      println(s"Number of injections with at least 1 misclassification: ${atLeast1SCInjections(BITFLIP).size}")
      println(s"Number of injections with only misclassifications: ${onlySCInjections(BITFLIP).size} ")

      val masked = alwaysMaskedInjections.mapValues(_.map(e => (convertInjectionToString(e._1), e._2)))
        .mapValues(_.keys.map(_.get)
          .groupBy(_.split('+').head)
          .mapValues(_.size))

      val observed = observedInjections.mapValues(_.map(e => (convertInjectionToString(e._1), e._2)))
        .mapValues(_.keys.map(_.get)
          .groupBy(_.split('+').head)
          .mapValues(_.size))

      val sc = atLeast1SCInjections.mapValues(_.map(e => (convertInjectionToString(e._1), e._2)))
        .mapValues(_.keys.map(_.get)
          .groupBy(_.split('+').head)
          .mapValues(_.size))

      val unionFind = network.bitFlipClasses(mkLeNetInputTFlow(LeNetFixtureSafecompPaper.input2D))
      val classesMasked = masked.mapValues(getEquivalentClasses(_, unionFind))
      println(s"Number of BITFLIP failure scenarios always masked = ${classesMasked(BITFLIP)._1}")
      println(s"Number of BITFLIP equivalent classes always masked = ${classesMasked(BITFLIP)._2.values.sum}")
      println(s"${classesMasked(BITFLIP)._2}")


      val observedClasses = observed.mapValues(getEquivalentClasses(_, unionFind))
      println(s"Number of BITFLIP failure scenarios observed = ${observedClasses(BITFLIP)._1}")
      println(s"Number of BITFLIP equivalent classes observed = ${observedClasses(BITFLIP)._2.values.sum}")
      println(s"${observedClasses(BITFLIP)._2}")


      val scClasses = sc.mapValues(getEquivalentClasses(_, unionFind))
      println(s"Number of BITFLIP failure scenarios with SC = ${scClasses(BITFLIP)._1}")
      println(s"Number of BITFLIP equivalent classes with SC = ${scClasses(BITFLIP)._2.values.sum}")
      println(s"${scClasses(BITFLIP)._2}")

    }
  }
  it should "get fault impact measures and compute the equivalence classes for Stuck at for misclassification" in {
    for {
      path <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(path), MNIST)
      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
      data <- restoreCheckpoint(model, false, "faultImpactSAMerged")
    } yield {
      val dataByType = data.mapValues(_.filterNot(_._2 == 0)).groupBy(_._1.faultType)

      val totalInjections = dataByType.mapValues(_.size)

      val misc = (1 to 9).map(i => dataByType.mapValues(
        _.filter(_._2.exists(_._1 match {
          case XMisclassification(x) => x == i
          case _ => false
        }))
          .filter(!_._2.exists(_._1 match {
            case XMisclassification(x) => x > i
            case _ => false
          }))))

      println(s"Total number of injections: ${totalInjections(STUCK_AT_0)} (SA-0), ${totalInjections(STUCK_AT_1)} (SA-1)")
      misc.zipWithIndex.foreach(m => println(s"Number of injections with at most ${m._2 + 1} misclassification: ${m._1(STUCK_AT_0).size}(SA-0) - ${m._1(STUCK_AT_1).size}(SA-1)"))
      val t = misc.map(_.map(_._2.size)).foldLeft((0, 0)) { (acc, d) => (acc._1 + d.head, acc._2 + d.last) }
      t._1 shouldBe 662
      t._2 shouldBe 1059
    }
  }
  it should "get fault impact measures and compute the misclassifications for Bitflip" in {
    for {
      path <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(path), MNIST, exclude = Some(Seq("stuck")))
      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D)
      campaignOthers = parseCampaign(new File("data/leNet5/results/bitflips/campaign.conf"), physicalModel)
      campaignInput = parseCampaign(new File("data/leNet5/results/bitflips_inputLayer/campaign.conf"), physicalModel)
      campaign = campaignInput ++ campaignOthers.map(p => (p._1 + campaignInput.size, p._2))
      input <- model.getLayerByName("conv2d")
      conv1 <- model.getLayerByName("max_pooling2d")
      pool1 <- model.getLayerByName("conv2d_1")
      conv2 <- model.getLayerByName("max_pooling2d_1")
      pool2 <- model.getLayerByName("dense")
      dense1 <- model.getLayerByName("dense_1")
      dense2 <- model.getLayerByName("dense_2")
      data <- restoreCheckpoint(model, false, "faultImpactBFMerged")
    } yield {
      val timeSpecs = retrieveTimeSpecs(model, campaign)
      timeSpecs.size shouldBe 7
      val rawDataByType = data
        .mapValues(_.filterNot(_._2 == 0)) // filter out the failure classes without impact
        .groupBy(_._1.faultType)
        .mapValues(_.filterNot(_._1.layerId.name.matches("dense"))) //filter out the bitflips incoherent with architecture (no registers)
        .mapValues(_.filterNot(q => q._1.layerId == input && q._1.startDate > 1024))
      val network = LeNetFixtureSafecompPaper
      val dictionary = Map(
        input -> timeSpecs(input).filterNot(_ > 1024).sorted.zip(network.imageInput.toList.sorted).toMap,
        conv1 -> timeSpecs(conv1).sorted.zip(network.conv1Output.toList.sorted).toMap,
        pool1 -> timeSpecs(pool1).sorted.zip(network.pool1Output.toList.sorted).toMap,
        conv2 -> timeSpecs(conv2).sorted.zip(network.conv2Output.toList.sorted).toMap,
        pool2 -> timeSpecs(pool2).sorted.zip(network.pool1Output.toList.sorted).toMap,
        dense1 -> timeSpecs(dense1).sorted.zip(network.seq1Output.toList.sorted).toMap,
        dense2 -> timeSpecs(dense2).sorted.zip(network.seq2Output.toList.sorted).toMap
      )
      val dataByType = rawDataByType.filter(_._1 == BITFLIP).mapValues(
        p => p.map(m => (m._1.copy(startDate = dictionary(m._1.layerId)(m._1.startDate)), m._2)))

      val totalInjections = dataByType.mapValues(_.size)
      val misc = dataByType.mapValues(l => (1 to 9).map(i =>
        l.filter(_._2.exists(_._1 match {
          case XMisclassification(x) => x == i
          case _ => false
        }))
          .filter(!_._2.exists(_._1 match {
            case XMisclassification(x) => x > i
            case _ => false
          }))))
      val miscRefined = misc.mapValues(m => m.zipWithIndex.map(p => p._1.groupBy(_._2(XMisclassification(p._2 + 1)))))
      val miscRefinedAll = misc.mapValues(m =>
        m
          .zipWithIndex
          .map(p =>
            p._1
              .groupBy(m => m._2.collect {
                case (XMisclassification(_), count) => count
              }.sum)
          )
      )

      val miscRefinedBounds = miscRefinedAll.mapValues(m => m.map(e => {
        val sizes = e.keys
        (sizes.min, sizes.max)
      }))
      FileWriter.writeCsvFile[String]("miscBoundsBF", miscRefinedBounds.map(m => (m._1.toString -> m._2.zipWithIndex.map(l => ((l._2 + 1).toString + "-misc", s"${l._1._1}->${l._1._2}")).toMap)), List("faultType") ++ (1 to 9).map(_.toString + "-misc").toList)


      println(s"Total number of injections: ${totalInjections(BITFLIP)}")
      misc.foreach(p => p._2.zipWithIndex.foreach(m => println(s"Number of ${p._1} injections resulting in ${m._2 + 1} misclassification: ${m._1.size}\n")))
    }
  }

  it should "get fault impact measures and compute the equivalence classes for Stuck at with  " in {
    for {
      path <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(path), MNIST)
      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
      data <- restoreCheckpoint(model, false, "faultImpactSAMerged")
    } yield {
      val dataByType = data.mapValues(_.filterNot(_._2 == 0)).groupBy(_._1.faultType)

      val totalInjections = dataByType.mapValues(_.size)

      val misc = dataByType.mapValues(l => (1 to 9).map(i =>
        l.filter(_._2.exists(_._1 match {
          case XMisclassification(x) => x == i
          case _ => false
        }))
          .filter(!_._2.exists(_._1 match {
            case XMisclassification(x) => x > i
            case _ => false
          }))))

      val miscRefined = misc.mapValues(m => m.zipWithIndex.map(p => p._1.groupBy(_._2(XMisclassification(p._2 + 1)))))
      val miscRefinedAll = misc.mapValues(m =>
        m
          .zipWithIndex
          .map(p =>
            p._1
              .groupBy(m => m._2.collect {
                case (XMisclassification(_), count) => count
              }.sum)
          )
      )

      val miscRefinedBounds = miscRefinedAll.mapValues(m => m.map(e => {
        val sizes = e.keys
        (sizes.min, sizes.max)
      }))
      FileWriter.writeCsvFile[String]("miscBounds", miscRefinedBounds.map(m => (m._1.toString -> m._2.zipWithIndex.map(l => ((l._2 + 1).toString + "-misc", s"${l._1._1}->${l._1._2}")).toMap)), List("faultType") ++ (1 to 9).map(_.toString + "-misc").toList)


      println(s"Total number of injections: ${totalInjections(STUCK_AT_0)} (SA-0), ${totalInjections(STUCK_AT_1)} (SA-1)")
      //      misc.zipWithIndex.foreach(m => println(s"Number of injections with at most ${m._2 + 1} misclassification: ${m._1(STUCK_AT_0).size}(SA-0) - ${m._1(STUCK_AT_1).size}(SA-1)"))

    }
  }

  it should "get the number of data with at least one misclassification" in {
    for {
      path <- extractResourceAsFile("leNet5")
      campaignFiles <- getCampaignFiles(new File(path), MNIST, exclude = Some(Seq("stuck")))
      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21).appendLeft(model.input2D)
      campaignOthers = parseCampaign(new File("data/leNet5/results/bitflips/campaign.conf"), physicalModel)
      campaignInput = parseCampaign(new File("data/leNet5/results/bitflips_inputLayer/campaign.conf"), physicalModel)
      campaign = campaignInput ++ campaignOthers.map(p => (p._1 + campaignInput.size, p._2))
      input <- model.getLayerByName("conv2d")
      conv1 <- model.getLayerByName("max_pooling2d")
      pool1 <- model.getLayerByName("conv2d_1")
      conv2 <- model.getLayerByName("max_pooling2d_1")
      pool2 <- model.getLayerByName("dense")
      dense1 <- model.getLayerByName("dense_1")
      dense2 <- model.getLayerByName("dense_2")
      data <- restoreCheckpoint(model, false, "faultImpactBFMerged")
    } yield {
      val timeSpecs = retrieveTimeSpecs(model, campaign)
      timeSpecs.size shouldBe 7
      val rawDataByType = data
        .mapValues(_.filterNot(_._2 == 0)) // filter out the failure classes without impact
        .groupBy(_._1.faultType)
        .mapValues(_.filterNot(_._1.layerId.name.matches("dense"))) //filter out the bitflips incoherent with architecture (no registers)
        .mapValues(_.filterNot(q => q._1.layerId == input && q._1.startDate > 1024))
      val network = LeNetFixtureSafecompPaper
      val dictionary = Map(
        input -> timeSpecs(input).filterNot(_ > 1024).sorted.zip(network.imageInput.toList.sorted).toMap,
        conv1 -> timeSpecs(conv1).sorted.zip(network.conv1Output.toList.sorted).toMap,
        pool1 -> timeSpecs(pool1).sorted.zip(network.pool1Output.toList.sorted).toMap,
        conv2 -> timeSpecs(conv2).sorted.zip(network.conv2Output.toList.sorted).toMap,
        pool2 -> timeSpecs(pool2).sorted.zip(network.pool1Output.toList.sorted).toMap,
        dense1 -> timeSpecs(dense1).sorted.zip(network.seq1Output.toList.sorted).toMap,
        dense2 -> timeSpecs(dense2).sorted.zip(network.seq2Output.toList.sorted).toMap
      )
      val dataByType = rawDataByType.filter(_._1 == BITFLIP).mapValues(
        p => p.map(m => (m._1.copy(startDate = dictionary(m._1.layerId)(m._1.startDate)), m._2)))

      val totalInjections = dataByType.mapValues(_.size)
      val misc = (1 to 9).map(i => dataByType.mapValues(
        _.filter(_._2.exists(_._1 match {
          case XMisclassification(x) => x == i
          case _ => false
        }))
          .filter(!_._2.exists(_._1 match {
            case XMisclassification(x) => x > i
            case _ => false
          }))))

      println(s"Total number of injections: ${totalInjections(BITFLIP)}")
      misc.zipWithIndex.foreach(m => println(s"Number of injections resulting in ${m._2 + 1} misclassification: ${m._1(BITFLIP).size}"))
      val t = misc.map(_.map(_._2.size)).foldLeft((0, 0)) { (acc, d) => (acc._1 + d.head, acc._2 + d.last) }
      println(t)
    }
  }
}