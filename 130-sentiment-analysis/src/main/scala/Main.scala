// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)
import org.apache.spark.ml.linalg.Vectors
import scala.collection.mutable.WrappedArray
import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.sql.functions._
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql._

import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.SparkContext

object Main {

  type Embedding       = (String, List[Double])
  type ParsedReview    = (Integer, String, Double)
  type EmbeddedReview    = (Integer, List[Double], Double)

  org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
  org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
  val spark =  SparkSession.builder
    .appName ("Sentiment")
    .config("spark.network.timeout", "20000s")
    .config("spark.executor.heartbeatInterval", "10000s")
    .master  ("local[9]")
    .getOrCreate

    import spark.implicits._

    val reviewSchema = StructType(Array(
      StructField ("reviewText", StringType, nullable=false),
      StructField ("overall",    DoubleType, nullable=false),
      StructField ("summary",    StringType, nullable=false)))

    // Read file and merge the text abd summary into a single text column

    def loadReviews (path: String): Dataset[ParsedReview] = {
      spark
        .read
        .schema (reviewSchema)
        .json (path)
        .rdd
        .zipWithUniqueId
        .map[(Integer,String,Double)] { case (row,id) => (id.toInt, s"${row getString 2} ${row getString 0}".toLowerCase, row getDouble 1) }
        .toDS
        .withColumnRenamed ("_1", "id" )
        .withColumnRenamed ("_2", "text")
        .withColumnRenamed ("_3", "overall")
        .as[ParsedReview]
    }

    // Load the GLoVe embeddings file

    def loadGlove (path: String): Dataset[Embedding] = {
      spark
        .read
        .text (path)
        .map  { _ getString 0 split " " }
        .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
        .withColumnRenamed ("_1", "word" )
        .withColumnRenamed ("_2", "vec")
        .as[Embedding]
    }

    def ratingToLabel(d : Double) = if (d < 2.5) 1 else if (d < 3.5) 2 else 3

    def prepareDataForTraining(reviews : Dataset[ParsedReview], glove : Dataset[Embedding]) = {
      val words = reviews.select('id, 'overall, explode(split('text, " ")).as("word"))
      val embedded = words.join(glove, words.col("word").equalTo(glove.col("word")))
      val pairRdd = embedded.rdd.map {
        case Row(id, overall, _, _, vec) => ((id,overall), (vec, 1))
      }
      val reduced = pairRdd.reduceByKey {
        case ((v1:WrappedArray[_], n1),(v2:WrappedArray[_], n2)) =>
          (v1.zip(v2).map {
            case ((a:Double,b:Double)) => a+b
          }, n1+n2)
      }

      reduced.map {
        case ((id:Int, overall:Double), (vec:WrappedArray[Double], n:Int))
        => (ratingToLabel(overall), Vectors.dense(vec.map((x:Double) => x/n).toArray))
      }.toDS
        .withColumnRenamed ("_1", "label" )
        .withColumnRenamed ("_2", "features")
    }

    def trainPerceptron(train : Dataset[Row], inputSize : Integer) = {
      new MultilayerPerceptronClassifier()
        .setLayers(Array[Int](inputSize, 4, 5, 4))
        .setBlockSize(128)
        .setSeed(1234L)
        .setMaxIter(100)
        .fit(train)
    }

    // def main(args: Array[String]) = {
    //   val sc = spark.sparkContext
    //   val data = MLUtils.loadLibSVMFile(sc, "sparktest.txt").toDF()
    //   val feature = data.first match {case Row(_, f) => f}
    //   println(feature)
    // }

    def main(args: Array[String]) = {
      val glove  = loadGlove ("glove.txt")
      val reviews = loadReviews ("musical_instruments.json")
      val dataset = prepareDataForTraining(reviews, glove)
      val datasets = dataset.randomSplit(Array(0.1, 0.9))
      val test = datasets(0)
      test.show
      val train = datasets(1)
      train.show
      val inputSize = glove.first match { case (_, v) => v.length }
      val model = trainPerceptron(train, inputSize)
      val result = model.transform(test)
      val predictionAndLabels = result.select("prediction", "label")
      //supported evaluators: "f1" (default), "weightedPrecision", "weightedRecall", "accuracy"
      val evaluator = new MulticlassClassificationEvaluator().setMetricName("accuracy")
      println("Precision:" + evaluator.evaluate(predictionAndLabels))
      spark.stop
    }
}
