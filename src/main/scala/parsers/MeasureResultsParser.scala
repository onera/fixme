package parsers


import org.apache.spark.sql.types.{DoubleType, IntegerType, StringType, StructType}
import org.apache.spark.sql.{DataFrame, SparkSession}

import java.io.File

object MeasureResultsParser {
  private val measureSchema = new StructType()
    .add("indicator", StringType)
    .add("failureCount", IntegerType)
    .add("total", IntegerType)
    .add("ratio", DoubleType)

  def parse(measureResultsFile: File) = {
    val spark = SparkSession.builder
      .master("local[*]")
      .appName("MeasureResultParser")
      .getOrCreate()

    spark
      .read
      .option("ignoreLeadingWhiteSpace", value = true)
      .option("ignoreTrailingWhiteSpace", value = true)
      .schema(measureSchema)
      .csv(measureResultsFile.getAbsolutePath)
      .na
      .drop()
  }

  private val faultImpactSchema = new StructType()
    .add("id", IntegerType)
    .add("layer", StringType)
    .add("channel", IntegerType)
    .add("bit", IntegerType)
    .add("faultType", StringType)
    .add("start", IntegerType)
    .add("stop", IntegerType)
    .add("SC-1", IntegerType)
    .add("SC-2", IntegerType)
    .add("SC-3", IntegerType)
    .add("SC-4", IntegerType)
    .add("SC-5", IntegerType)
    .add("SC-6", IntegerType)
    .add("SC-7", IntegerType)
    .add("SC-8", IntegerType)
    .add("SC-9", IntegerType)
    .add("Observed", IntegerType)
    .add("Masked", IntegerType)

  def parseFaultImpactCheckPoints(measureResultsFiles: File*): DataFrame = {
    val spark = SparkSession.builder
      .master("local[*]")
      .appName("FaultImpactCheckPointsParser")
      .getOrCreate()

    spark
      .read
      .option("ignoreLeadingWhiteSpace", value = true)
      .option("ignoreTrailingWhiteSpace", value = true)
      .option("header", value = true)
      .option("inferSchema", value = true)
      //  .schema(faultImpactSchema)
      .csv(measureResultsFiles.map(_.getAbsolutePath): _*)
      .na
      .drop()
  }
}
