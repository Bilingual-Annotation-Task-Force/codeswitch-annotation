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
    val enModel = new NgramModel("Eng", getConditionalCounts(engData, n), n)
    val esModel = new NgramModel("Span", getConditionalCounts(spanData, n), n)
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, esModel))
    val eval = new Evaluator(cslm)
    eval.annotate("Killer_Cronicas")
    println("accuracy: " + eval.evaluate("Solorio_7k"))
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
    
    

  
}


	



	


