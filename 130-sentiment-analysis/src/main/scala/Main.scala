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


object Main {

	type Embedding       = (String, List[Double])
	type ParsedReview    = (Integer, String, Double)
	type EmbeddedReview    = (Integer, List[Double], Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[9]")
		.getOrCreate

  import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text abd summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
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

  // Load the GLoVe embeddings file

  def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
      .map  { _ getString 0 split " " }
      .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]


  def embed(tokens:WrappedArray[String])(embedding: Dataset[Embedding]): List[Double] = {
    List()

  }

  def tokenize (data: Dataset[ParsedReview])(glove: Dataset[Embedding]) = {
    // val tokenizer = new Tokenizer();
    // val tokenized = tokenizer.setInputCol("text").setOutputCol("tokens").transform(data);
    // val embeddedCol = udf ({ (x:List[String]) => embed(x)(glove) }).apply(tokenized.col("tokens"));
    // val embedded = data.withColumn("embedded", embeddedCol);

    // val embeddings = tokenized.toDF.select("tokens").rdd.map {
      // case Row(l: WrappedArray[String]) => embed(l)(glove)
    // }.toDS.col("value")
    // tokenized.withColumn("embedded", embeddings)
  }

  def ratingToLabel(d : Double) = {
    if (d < 2.5) {
      1
    } else if (d < 3.5) {
      2
    } else {
      3
    }
  }

  def trainPerceptron(train : Dataset[Row], inputSize : Integer) = {
    val layers = Array[Int](inputSize, 3, 3, 1)
    val trainer = new MultilayerPerceptronClassifier()
    .setLayers(layers)
    .setBlockSize(128)
    .setSeed(1234L)
    .setMaxIter(100)

    val model = trainer.fit(train)
    model
  }


  def main(args: Array[String]) = {

    val glove  = loadGlove ("glove.txt")
    glove.show
    val reviews = loadReviews ("musical_instruments.json")

    // replace the following with the project code
    val words = reviews.select('id, 'overall, explode(split('text, " ")).as("word"))
    val embedded = words.join(glove, words.col("word").equalTo(glove.col("word")))
    val pairRdd = embedded.rdd.map {
      case Row(id, overall, _, _, vec) => ((id,overall), (vec, 1))
    }
    val reduced = pairRdd.reduceByKey {
      case ((v1:WrappedArray[Double], n1),(v2:WrappedArray[Double], n2)) =>
        (v1.zip(v2).map {
          case ((a:Double,b:Double)) => a+b
        }, n1+n2)
    }

    val dataset =  reduced.map {
      case ((id:Int, overall:Double), (vec:WrappedArray[Double], n:Int))
        => (ratingToLabel(overall), Vectors.dense(vec.map((x:Double) => x/n).toArray))
    }.toDS
     .withColumnRenamed ("_1", "label" )
     .withColumnRenamed ("_2", "features")

    val datasets = dataset.randomSplit(Array(0.1, 0.9))
    val test = datasets(0)
    val train = datasets(1)
    val inputSize = glove.first match {
      case (_, v) => v.length
    }

    val model = trainPerceptron(train, inputSize)
    // embedded.groupBy('id, 'overall).agg(array("vec")).show

    spark.stop
  }

}
