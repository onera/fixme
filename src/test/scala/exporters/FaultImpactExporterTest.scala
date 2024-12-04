package exporters

//
//class FaultImpactExporterTest extends AnyFlatSpec with ScalaCheckPropertyChecks with should.Matchers {
//
//  it should "process real campaign data" in {
//
//    for {
//      path <- extractResourceAsFile("leNet5")
//      campFiles <- getCampaignFiles(new File(path), 0 to 3, Some("bitflips"))
//
//    } yield {
//      val resultIterator = campFiles.resultsFiles
//        .map(p => Parser.parse(p._2, p._1, campFiles.modelFile, campFiles.labelDictionary)).toSeq
//      val resultIterator2 = campFiles.resultsFiles
//        .map(p => Parser.parse(p._2, p._1, campFiles.modelFile, campFiles.labelDictionary)).toSeq
//      val res = FaultImpactExporter.export("test1", resultIterator, false)
//      res.size shouldBe 2400
//      res.foreach {
//        _._2.values.sum shouldBe 4
//      }
//
//    }
//  }
//
//  it should "accumulate failure classes by injection" in {
//    val injectPoint = Injection(Convolution2D("conv", Shape2D(1, 2, 3), Shape2D(1, 2, 3), Shape2D(1, 2, 3), true, 3), 5, 1, BITFLIP, 1, 2)
//    val activation1 = DatasetItem(1, "2")
//    val activation2 = DatasetItem(4, "5")
//    val c1 = CampaignResult(injectPoint, activation1, Map("0" -> 1, "1" -> 2, "2" -> 3), Map("0" -> 1, "1" -> 2, "2" -> 3))
//    val c2 = CampaignResult(injectPoint, activation2, Map("0" -> 1, "1" -> 2, "2" -> 3), Map("0" -> 1, "1" -> 2, "2" -> 3))
//    val resultIterator = Seq(c1, c2).map(Seq(_).toIterator)
//    val res = FaultImpactExporter.export("test2", resultIterator)
//    res.size shouldBe 1
//    res.head._2.size shouldBe 1
//    res.head._2.head._1 shouldBe Masked
//    res.head._2.head._2 shouldBe 2
//  }
//
//
//  it should "export the classifications of each injection on lenet5" in {
//    for {
//      campaignFiles <- getCampaignFiles(new File("data/leNet5"), exclude = Some("bitflip"))
//      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
//      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
//    } yield {
//      val filesOrdered = campaignFiles
//        .resultsFiles
//        .foldLeft(Seq.empty[(File, File)]) { (l, r) =>
//          r._2
//            .map(
//              f => (r._1, f)
//            ) ++ l
//        }
//        .toList
//      //.sortBy(_._2.getName)
//      val cpStep = 250
//      val availableProcessors = Runtime.getRuntime.availableProcessors()
//      println(s"processing ${campaignFiles.resultsFiles.map(_._2.size).sum} results files using ${availableProcessors} processors with ${cpStep} checkpoints.")
//
//      val batchSize = filesOrdered.size / cpStep
//
//      val filesGrouped = filesOrdered
//        .grouped(batchSize)
//        .toList
//        .map(p => {
//          val threadSize = if (p.size >= availableProcessors) {
//            p.size / availableProcessors
//          } else
//            1
//          p.grouped(threadSize).toList
//        })
//
//
//      /*generate the corresponding sequence of results iterator (for parallelization)*/
//      filesGrouped.zipWithIndex.foreach(batch => {
//        val resultIterator = batch._1.map(p => Parser.parse(p, campaignFiles.modelFile, campaignFiles.labelDictionary))
//        // export results
//        FaultImpactExporter.export(s"faultImpactBF-${batch._2}", resultIterator, true, true)
//      })
//      /* merge results files into one */
//      for {
//        mergedResults <- FaultImpactExporter.restoreCheckpoint(model, true, filesGrouped.indices.map(i => s"faultImpactBF-${i}"): _*)
//      } yield
//        FaultImpactExporter.writeFile(mergedResults, "faultImpactBFMerged")
//    }
//  }
//
//  it should "merge bitflips results into temp file" in {
//    for {
//      campaignFiles <- getCampaignFiles(new File("data/leNet5"))
//      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
//      physicalModel = model.sequential("dense", 10).sequential("dense_1", 21)
//    } yield {
//      (0 to 555).sliding(10, 10).toList.foreach(p =>
//        for {
//          mergedResults <- FaultImpactExporter.restoreCheckpoint(model, true, (p.head to p.last).map(i => s"faultImpactBF-${i}"): _*)
//        } yield {
//          FaultImpactExporter.writeFile(mergedResults, s"faultImpactBFMerged-${p.head}-${p.last}", true)
//        }
//      )
//    }
//  }
//
//  def getListOfFilesInDirectory(directory: File): Option[List[String]] = {
//    if (directory.exists && directory.isDirectory) {
//      Some(directory.listFiles.filter(_.isFile).toList.map(_.getName.split('.').head))
//    } else {
//      None
//    }
//  }
//
//  it should "merge bitflips results into 1 file" in {
//    for {
//      campaignFiles <- getCampaignFiles(new File("data/leNet5"))
//      model <- parsers.CNNJsonParser.parseModel(campaignFiles.modelFile)
//      files <- getListOfFilesInDirectory(new File("output/measures/tmp"))
//      mergedResults <- FaultImpactExporter.restoreCheckpoint(model, true, files.filter(p => p.contains("faultImpactBFMerged")): _*)
//    } yield {
//      FaultImpactExporter.writeFile(mergedResults, s"faultImpactBFMerged")
//    }
//  }
//}
