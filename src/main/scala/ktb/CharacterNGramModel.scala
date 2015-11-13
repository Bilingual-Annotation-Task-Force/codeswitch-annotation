package ktb

import scala.io.Source
import java.io._

object CharacterNGramModel {
  
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
        case (ctx,charCounts) => ctx -> charCounts.mapValues(count => (count + 1) / (contextCountTotals(ctx).toDouble + 26))
      }
      
      def getLanguage(): String = { language }
      
      def prob(ctx: String, c: String): Double = {
        normalizedCounts.getOrElse(ctx, Map.empty).getOrElse(c, 1.0 / (contextCountTotals.getOrElse(ctx, 0).toDouble + 26))
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
      
      def printConditionalCounts() = {
        var output = new PrintStream("conditionalCounts")
        output.println("num ngrams: " + (conditionalCounts.keys).size)
        conditionalCounts foreach { 
          case (ctx, counts) => {
             output.println(ctx + "-->")
             counts foreach { case (char, num) => output.println(char + ": " + num) }
          }
        }
      }
      
    }
}

