package ktb

import scala.io.Source

/*
 * accuracy w/ DQ: 
 * n = 3: 0.8802083333333334
 * n = 5: 0.9195075757575758
 *
 */

object NGrams {

  def main(args: Array[String]): Unit = {
    
    val engData = Source.fromFile("dq-en.txt").getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).map(_.toLowerCase()).toVector
    val spanData = Source.fromFile("dq-es.txt").getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).map(_.toLowerCase()).toVector
    val n = 5
    
    val enModel = new NgramModel("Eng", getConditionalCounts(engData, n), n)
    val esModel = new NgramModel("Span", getConditionalCounts(spanData, n), n)
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, esModel))
    
    val accuracy = cslm.evaluate("tagged_codeswitched_data.txt")
    println("accuracy: " + accuracy)

  } 
  
       
  def getNgrams(string: String, n: Int): Iterator[String] = {
    val stringWithStartEnd = (" " * (n - 1)) + string + " "
    stringWithStartEnd.sliding(n)
  }
  
  def getConditionalCounts(strings: Vector[String], n: Int): Map[String, Map[String, Int]] = {
    val ngrams = strings.flatMap(s => getNgrams(s, n))
    ngrams.groupBy(_.take(n - 1)).map {
      case (ctx, trigramOccurrences) =>
        val lastChars = trigramOccurrences.map(_.drop(n - 1))
        val lastCharCounts = lastChars.groupBy(x => x).map {
         case (lastChar, lastCharOccurences) => lastChar -> lastCharOccurences.size
        }
        ctx -> lastCharCounts
    }
  }
    
  class NgramModel(
    language: String,
    conditionalCounts: Map[String, Map[String, Int]],
    val n: Int) {
  
    private[this] val contextCountTotals = conditionalCounts.map {
      case (ctx, lastCharCounts) => ctx -> lastCharCounts.map(_._2).sum
    }
  
    private[this] val normalizedCounts = conditionalCounts.map { 
      case (ctx,charCounts) => ctx -> charCounts.mapValues(count => count / contextCountTotals(ctx).toDouble )
    }
    
    def getLanguage(): String = { language }
    
    def prob(ctx: String, c: String): Double = {
      normalizedCounts.getOrElse(ctx, Map.empty).get(c).map(x => x + 1.0).getOrElse(1.0)
    }
    
    def stringProb(string: String): Double = {
      val ngramProbs =
        for {
          (ctx, counts) <- getConditionalCounts(Vector(string), n)
          (lastChar, count) <- counts
        } yield {
          prob(ctx, lastChar) * count
        }
      ngramProbs.product
    }
    
  }
  
  class codeSwitchedLanguageModel(
      models: Vector[NgramModel] ) {
    
    def guess(string: String) : String = {
      val highestProb = models.map(_.stringProb(string)).max
      val guess = models.filter(x => x.stringProb(string) == highestProb)
      return guess(0).getLanguage()
    }
    
    def evaluate(filename: String) : Double = {
      var correct = 0
      var total = 0
      val lines = Source.fromFile(filename).getLines()
      lines.foreach { x =>
        val tokens = x.split(",") 
        val word = tokens(0).toLowerCase()
        if (word.charAt(0) >= 'a' && word.charAt(0) <= 'z') { //don't evaluate "other" (punctuation, etc.)
           val correctLang = tokens(tokens.length - 1)
           val guessedLang = guess(word)
           if (guessedLang == correctLang)
             correct += 1
           total += 1    
        }
      }
      return correct.toDouble / total.toDouble
    }
  }
    
  
}
