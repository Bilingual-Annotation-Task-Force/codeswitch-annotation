package ktb

import scala.io.Source
import java.io._

import edu.stanford.nlp.ie.AbstractSequenceClassifier;
import edu.stanford.nlp.ie.crf._;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations;


/*
 * accuracy w/ DQ: 
 * n = 3: 0.8802083333333334
 * n = 5: 0.9195075757575758
 * with accents and n = 5: 0.9302656546489564
 *
 * accuracy w/ Mexican Corpus and 1/3 of English Corpus:
 * n= 3: 0.9018026565464896
 * n = 5: 0.9381720430107527
 * 
 * accuracy w/ incorporated NER:
 * n = 5: 0.9401750198886237
 * 
 */

object NGrams {

  def main(args: Array[String]): Unit = {
    
    val engData = Source.fromFile("EngCorpus-1m.txt").getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).map(_.toLowerCase()).toVector
    val spanData = Source.fromFile("MexCorpus.txt").getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).map(_.toLowerCase()).toVector
    val n = 5 
    
    val enModel = new NgramModel("Eng", getConditionalCounts(engData, n), n)
    val esModel = new NgramModel("Span", getConditionalCounts(spanData, n), n)
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, esModel))
    
    val accuracy = cslm.evaluate("Solorio_GoldSt_7k-retagged.txt")
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
      
      var outputFile = new FileOutputStream("output.txt")
      var output = new PrintStream(outputFile)
      output.println("Word\tCorrect Language\tGuessed Language")
      
      // initialize named entity recognizer (NER)
      var serializedClassifier = "classifiers/english.nowiki.3class.distsim.crf.ser.gz"
      var classifier = CRFClassifier.getClassifier(serializedClassifier);
  
      // counts of correct and total guesses to calculate accuracy
      var correct = 0
      var total = 0
      val lines = Source.fromFile(filename).getLines()
      
      lines.foreach { x =>
        val tokens = x.split(",")
        output.print(tokens(0) + " ")
        val word = tokens(0).toLowerCase()
        if (word.charAt(0) >= 'a' && word.charAt(0) <= 'z') { 
          
           // check to see if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
           var classification = classifier.classifyWithInlineXML(word)
           if (classification.charAt(0) == '<'){
             var tag = classification.split('>')(0)
             output.println("NAMED ENTITY - " + tag + ">")
           }
           
           // else, guess the language using n-gram models and update count of correct guesses
           else {
             val correctLang = tokens(tokens.length - 1)
             val guessedLang = guess(word)
             if (guessedLang == correctLang)
               correct += 1
             total += 1
             output.print(correctLang + " " + guessedLang + " \n")
           }
          
        }
        // otherwise, word is punctuation and should not be evaluated
        else {
          output.print(tokens(tokens.length - 1) + " Other\n")
        }
      }
      
      // caluclate and return accuracy
      return correct.toDouble / total.toDouble
    }
  }
    
  
}
