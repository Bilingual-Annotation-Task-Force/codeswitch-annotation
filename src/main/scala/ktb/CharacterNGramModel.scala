package ktb

import scala.io.Source
import java.io._

object CharacterNGramModel {
  
  /* Splits string into character n-grams of length n
   * @param string the training data
   * @param n the n-gram length
   */
  def getNgrams(string: String, n: Int): Iterator[String] = {
    val stringWithStartEnd = (" " * (n - 1)) + string + " "
    stringWithStartEnd.sliding(n)
  }
  
  
  /* Creates a map of conditional frequencies in which the keys are unique n-1 grams (the first n-1 characters of each ngram), and the values
   * are a map of characters that follow that n-1 gram to the frequency with which that character appears
  
     Ex: string = "This is a sentence", n = 3
     
     ngrams: ["  T", " Th", "Thi", "his", "is ", "s i", " is", "is ", "s a", " a ", "a s", " se", "sen", "ent", "nte", "ten", "enc", nce", "ce "]
     
     Returns:
      ctx: "  ", lastCharCounts: Map(T -> 1)
      ctx: "se", lastCharCounts: Map(n -> 1)
      ctx: "nc", lastCharCounts: Map(e -> 1)
      ctx: "is", lastCharCounts: Map(  -> 2)
      ctx: "s ", lastCharCounts: Map(a -> 1, i -> 1)
      ctx: "Th", lastCharCounts: Map(i -> 1)
      ctx: " a", lastCharCounts: Map(  -> 1)
      ctx: " s", lastCharCounts: Map(e -> 1)
      ctx: "ce", lastCharCounts: Map(  -> 1)
      ctx: "en", lastCharCounts: Map(t -> 1, c -> 1)
      ctx: " T", lastCharCounts: Map(h -> 1)
      ctx: "hi", lastCharCounts: Map(s -> 1)
      ctx: "te", lastCharCounts: Map(n -> 1)
      ctx: "a ", lastCharCounts: Map(s -> 1)
      ctx: " i", lastCharCounts: Map(s -> 1)
      ctx: "nt", lastCharCounts: Map(e -> 1)
      

   */
  def getConditionalCounts(strings: Vector[String], n: Int): Map[String, Map[String, Int]] = {
   
    val ngrams = strings.flatMap(s => getNgrams(s, n))
    
    // Groups ngrams by first n-1 characters (ctx) and maps them to a list of all ngrams beginning with that context
    ngrams.groupBy(_.take(n - 1)).map {
      
      // For each such mapping, group the list of ngrams by their last character, and re-map the context to the 
      // size of the group (i.e., the number of ngrams that end in that character) 
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
    
      /* 
       * Creates a map of n-1 length grams (ctx) to their total frequency
       */
      private[this] val contextCountTotals = conditionalCounts.map {
        case (ctx, lastCharCounts) => ctx -> lastCharCounts.map(_._2).sum
      }
    
      /*
       * For each n-1 length gram (ctx), creates a mapping of last characters to their relative frequency, normalized by the total frequency of that context
       * Adds 1 to each count to prevent frequency 0; adds 26 to total count to normalize (one for each potentially unseen last character)
       * 
       * (Note: this should be parameterized for the number of characters in that language) 
       */
      private[this] val normalizedCounts = conditionalCounts.map { 
        case (ctx,charCounts) => ctx -> charCounts.mapValues(count => (count + 1) / (contextCountTotals(ctx).toDouble + 26))
      }
      
      /*
       * Returns the name of the language
       */
      def getLanguage(): String = { language }
      
      /*
       * Returns the probability of seeing a character (@param c) given the previous n-1 characters (@param ctx)
          i.e., P(c | ctx)      
       
         Ex: Using an trigram model of "This is a sentence", what's the probability of seeing the the character "t"
         given that you've seen the characters "en"?
         
          probability = #("ent") / #("en"x); # = number of occurences of, x = any character
          probability = 1 / 2 
                   
       */
      def prob(ctx: String, c: String): Double = {
        normalizedCounts.getOrElse(ctx, Map.empty).getOrElse(c, 1.0 / (contextCountTotals.getOrElse(ctx, 0).toDouble + 26))
      }
      
      
      /* 
       * Calculates the probability of a string by multiplying the probabilities of each ngram
       */
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
      
      /* 
       * used for debugging
       */
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

