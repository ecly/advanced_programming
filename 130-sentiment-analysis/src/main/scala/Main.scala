// AUTHORS:
// miev@itu.dk
// ecly@itu.dk

import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.ml.Transformer
import scala.collection.mutable.WrappedArray
import org.apache.spark.ml.Pipeline
import org.apache.spark.ml.tuning.{ParamGridBuilder, CrossValidator}
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.sql.functions._
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._
import org.apache.spark.sql._

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

    def ratingToLabel(d : Double) = if (d < 2.5) 0 else if (d < 3.5) 1 else 2

    def prepareDataForTraining(reviews : Dataset[ParsedReview], glove : Dataset[Embedding]) = {
      val words = reviews.select('id, 'overall, explode(split('text, " ")).as("word"))
      val embedded = words.join(glove, words.col("word").equalTo(glove.col("word")))
      val pairRdd = embedded.rdd.map {
        case Row(id, overall, _, _, vec) => ((id,overall), (vec, 1))
      }
      val reduced = pairRdd.reduceByKey {
        case ((v1:WrappedArray[_], n1),(v2:WrappedArray[_], n2)) =>
          (v1.zip(v2).map { case ((a:Double,b:Double)) => a+b }, n1+n2)
      }

      reduced.map {
        case ((id:Int, overall:Double), (vec:WrappedArray[Double], n:Int))
          => (ratingToLabel(overall), Vectors.dense(vec.map((x:Double) => x/n).toArray))
      }.toDS
        .withColumnRenamed ("_1", "label" )
        .withColumnRenamed ("_2", "features")
    }

    // Create simply MultilayerPerceptronClassifier with a somewhat
    // arbitrary selection of hidden layer.
    def perceptronClassifier(inputSize : Integer) = {
      new MultilayerPerceptronClassifier()
        .setLayers(Array[Int](inputSize, 4, 5, 3))
        .setBlockSize(128)
        .setSeed(1234L)
        .setMaxIter(100)
    }

    // Supports: "f1", "weightedPrecision", "weightedRecall", "accuracy"
    val evaluatorF1 = new MulticlassClassificationEvaluator().setMetricName("f1")
    def evaluateF1(model : Transformer, testSet : Dataset[_]) = {
      val result = model.transform(testSet)
      val predictionAndLabels = result.select("prediction", "label")
      evaluatorF1.evaluate(predictionAndLabels)
    }

    def evaluatePerceptronBasic(dataset : Dataset[_], inputSize : Int) = {
      val datasets = dataset.randomSplit(Array(0.9, 0.1))
      val classifier = perceptronClassifier(inputSize)
      val model = classifier.fit(datasets(0))
      evaluateF1(model, datasets(1))
    }

    // Adapted from CrossValidationExample from sparks official GitHub repo.
    def evaluatePerceptronKFoldCV(dataset : Dataset[_], inputSize : Int, k : Int) = {
        val datasets = dataset.randomSplit(Array(0.9, 0.1))
        val classifier = perceptronClassifier(inputSize)
        val pipeline = new Pipeline().setStages(Array(classifier))
        val paramGrid = new ParamGridBuilder().build()
        val cv = new CrossValidator()
          .setEstimator(pipeline)
          .setEvaluator(evaluatorF1)
          .setEstimatorParamMaps(paramGrid)
          .setNumFolds(k)
        val model = cv.fit(datasets(0))
        evaluateF1(model, datasets(1))
      }

    def main(args: Array[String]) = {
      val glove  = loadGlove ("glove.txt")
      val inputSize = glove.first match { case (_, v) => v.length }
      val reviews = loadReviews ("musical_instruments.json")
      val dataset = prepareDataForTraining(reviews, glove)
      val basicF1 = evaluatePerceptronBasic(dataset, inputSize)
      val crossF1 = evaluatePerceptronKFoldCV(dataset, inputSize, 10)
      println("F1-score-basic:" + basicF1)
      println("F1-score-10fold-CV:" + crossF1)
      spark.stop
    }
}
