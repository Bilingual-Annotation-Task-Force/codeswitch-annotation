package ktb


import scala.io.Source
import java.io._
import ktb.CharacterNGramModel._
import edu.stanford.nlp.ie.AbstractSequenceClassifier;
import edu.stanford.nlp.ie.crf._;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations;


object Evaluation {
  
  def toWords(lines: Vector[String]) = lines flatMap { line =>
    line.replaceAll("\\t|\\n|\\r|\\p{P}", "").split(" ").filter(w => !w.isEmpty()) map (_.toLowerCase)
  }
  
  def main(args: Array[String]): Unit = {
    val n = 5 
    val engData = toWords(Source.fromFile("EngCorpus-1m.txt").getLines().toVector)
    val spanData = toWords(Source.fromFile("MexCorpus.txt").getLines().toVector)
    val enModel = new NgramModel("english", getConditionalCounts(engData, n), n)
    val esModel = new NgramModel("spanish", getConditionalCounts(spanData, n), n)
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, esModel))
    val eval = new Evaluator(cslm)
    //eval.annotate("Killer_Cronicas")
    //println("accuracy: " + eval.evaluate("Solorio_7k"))
    eval.evalWithHmm("Killer_Cronicas")
  }


}

class Evaluator(cslm : codeSwitchedLanguageModel) {
  
    private[this] val classifier = CRFClassifier.getClassifier("classifiers/english.nowiki.3class.distsim.crf.ser.gz");
  
    /* Evaluating unannotated text (Killer Cronicas)
    */
    def annotate(filename: String) = {
      var output = new PrintStream(new FileOutputStream(filename + "-output.txt"))
      output.println("Token,Tag")

      val tokens = Source.fromFile(filename).getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).toVector
      tokens.foreach { x =>
        output.print(x + ",")
        val word = x.toLowerCase()
        if (word.charAt(0) >= 'a' && word.charAt(0) <= 'z') { 
           // check to see if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
           var classification = classifier.classifyWithInlineXML(word)
           if (classification.charAt(0) == '<'){
             output.println("NAMED ENTITY")
           }              
           else {
             val guessedLang = cslm.guess(word)
             output.println(guessedLang)
           }
          
        }
        // otherwise, word is punctuation and should not be evaluated
        else {
          output.println("OTHER")
        }
      }

    }
    
   /* evaluate accuracy against gold standard (Solorio's)
   */   
    def evaluate(filename: String) : Double = {

      var outputFile = new FileOutputStream(filename + "-output.txt")
      var output = new PrintStream(outputFile)
      val lines = Source.fromFile(filename).getLines()
      
      // counts to calculate accuracy
      var correct = 0
      var total = 0
      
      lines.foreach { x =>
        val tokens = x.split(",")
        output.print(tokens(0) + ",")
        val word = tokens(0).toLowerCase()
        if (word.charAt(0) >= 'a' && word.charAt(0) <= 'z') { 
          
           // check to see if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
           var classification = classifier.classifyWithInlineXML(word)
           if (classification.charAt(0) == '<'){
             output.println("NAMED ENTITY")
           }
           
           // else, guess the language using n-gram models and update count of correct guesses
           else {
             val correctLang = tokens(tokens.length - 1)
             val guessedLang = cslm.guess(word)
             output.print(guessedLang + ",")
             if (guessedLang == correctLang){
               correct += 1
               output.println("CORRECT")
             }
             else {
               output.println("INCORRECT")
             }
             total += 1
           }
          
        }
        // otherwise, word is punctuation and should not be evaluated
        else {
          output.println("Other")
        }
      }
      
      return correct.toDouble / total.toDouble
    }
    
    def evalWithHmm(filename: String) = {
      val words = Source.fromFile(filename).getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).toArray
      var output = new PrintStream(new FileOutputStream(filename + "-outputwithHMM.txt"))
      val tags = Array("english", "spanish")
      val engprobs = Array(0.6, 0.4)
      val spanprobs = Array(0.4, 0.6)
      val eng = tags zip engprobs toMap 
      val span = tags zip spanprobs toMap
      val transitions = (tags zip Array(eng, span)) toMap
      
      val model = new HMM(words, tags, transitions, cslm)
      model.viterbi()
      output.println(model.retrace())
      println("finished")
    }
 
}


class HMM(words: Array[String], tagSet: Array[String], transitions: Map[String, Map[String, Double]], cslm: codeSwitchedLanguageModel) {
  
  val v = Array.ofDim[Node](words.length, tagSet.length)
  
  def em(ctx: String, word: String) : Double = {
    cslm.prob(ctx, word)
  }
  
  def tr(ctx: String, tag: String) : Double = {
    transitions.get(ctx).get(tag)
  }
  
  def viterbi() = {
    
      // init; equal probability of starting with either tag
      for (tag <- 0 until tagSet.length) {
        v(0)(tag) = new Node(0.5, tag) 
      }

      for (word <- 1 until words.length) {
        for (tag <- 0 until tagSet.length) {
          //println(tagSet(tag), words(word))
          val priors = new Array[Node](tagSet.length)
          for (prevTag <- 0 until tagSet.length) {
            val transitionProb = v(word-1)(prevTag).getProb * tr(tagSet(prevTag), tagSet(tag))
            priors(prevTag) = new Node(transitionProb, prevTag)
            //println("from " + prevTag + " to " + tag + ": " + transitionProb)
          }
          
          // find maximally probable previous tag
          var max = priors(0)
          for (t <- 1 until tagSet.length) {
            if (priors(t).getProb > max.getProb)
              max = priors(t)
          }
          //println("maxProb : " + max.getProb + ", maxPrev: " + max.getPrev)
          val emissionProb = em(tagSet(tag), words(word))
          //println("emission " + word + " under " + tag + ": " + emissionProb)
          v(word)(tag) = new Node(emissionProb * max.getProb, max.getPrev)     
          //println("v(" + words(word) + ")(" + tagSet(tag) + "): " + v(word)(tag).getProb)
        }       
      }
    
  }
  def retrace() : String = {
    val tags = new Array[String](words.length)
    val last = if (v(words.length - 1)(0).getProb > v(words.length - 1)(1).getProb) 0 else 1
    tags(words.length - 1) = tagSet(last)
    var prev = v(words.length - 1)(last).getPrev
    for (k <- (words.length - 2) to 0 by -1){
      tags(k) = tagSet(prev)
      prev = v(k)(prev).getPrev  
    }
    (words zip tags).mkString(" ")
  }
  
  class Node(prob: Double, prevTag: Int) {
      def getProb() : Double = { prob }
      def getPrev() : Int = { prevTag }
  }
      
}


	



	


