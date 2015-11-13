package ktb

import scala.io.Source
import java.io._
import ktb.CharacterNGramModel._
import edu.stanford.nlp.ie.AbstractSequenceClassifier;
import edu.stanford.nlp.ie.crf._;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations;



/**
 * @author kelseyball
 */


object Evaluation_BonCop { 
  
  def toWords(lines: Vector[String]) = lines flatMap { line =>
    line.replaceAll("\\t|\\n|\\r|\\p{P}", "").split(" ").filter(w => !w.isEmpty()) map (_.toLowerCase)
  }
  
  
  def main(args: Array[String]): Unit = {
    
    val testCorpus = "BonCop/BonCopTokensByLine"
    val goldStandard = "BonCop/BonCopGoldStandard"
    
    // Train two n-gram models on English and Spanish oral corpora
    val n = 5 
    val engData = toWords(Source.fromFile("TrainingCorpora/EngCorpus-1m.txt").getLines().toVector)
    val frData = toWords(Source.fromFile("TrainingCorpora/(encoding utf-8)presse.txt", "UTF-8").getLines().toVector)
    var output = new PrintStream("frData")
    frData foreach output.println
    val enModel = new NgramModel("Eng", getConditionalCounts(engData, n), n)
    val frModel = new NgramModel("Fr", getConditionalCounts(frData, n), n)
    frModel.printConditionalCounts
    
    // Build code-switched language model from English and Spanish n-gram models
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, frModel))
  
    // Build Hidden Markov Model with estimated transition probabilities (emission probabilities given by code-switched language model)
    val testWords = Source.fromFile(testCorpus, "UTF-8").getLines().toArray
    // to do: grab tabs from cslm languages
    val tags = Array("Eng", "Fr")
    val transitions = (tags zip Array(tags zip Array(0.87, 0.13) toMap, tags zip Array(0.13, 0.87) toMap)) toMap
    val hmm = new HiddenMarkovModel(testWords, tags, transitions, cslm)

   
    // Build evaluator using code switched language model and hidden markov model
    val eval = new Evaluator(cslm, hmm)
    //eval.annotate(testCorpus)
    print(eval.evaluate(goldStandard))


  }




class Evaluator(cslm : codeSwitchedLanguageModel, hmm: HiddenMarkovModel) {
  
    private[this] val engClassifier = CRFClassifier.getClassifier("classifiers/english.nowiki.3class.distsim.crf.ser.gz");

    def annotate(filename: String) = {

      var output = new PrintStream(filename + "_annotated.txt", "UTF-8")
      output.println("Token,Tag")
      val hmmtags = hmm.generateTags()
      val words = Source.fromFile(filename).getLines().toArray  
      println("hmmtags len:  " + hmmtags.length)
      println("len words:  " + words.length)
      for (k <- 0 until words.length) {
        var guess = hmmtags(k)
        var word = words(k)
        //var guess = cslm.guess(word)
        // Check if word is punctuation
        if (word.matches("\\p{P}")) {
          guess = "Punct"
        }
        
        // Check if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
        var engClassification = engClassifier.classifyWithInlineXML(word)
        //to do: add French classifier
        if (engClassification.contains('<')){
          guess = "NamedEnt"
        }
        
        output.println(word + "," + guess)

      }

    }
    
    def evaluate(goldStandard: String) : Double = {
      
      var outputFile = new FileOutputStream(goldStandard + "_outputWithHMM.txt")
      var output = new PrintStream(outputFile)
      output.println("Word \t Guess \t Tag \t Correct/Incorrect")
      val lines = Source.fromFile(goldStandard).getLines().toArray
      val hmmtags = hmm.generateTags()
 
      // counts to calculate accuracy
      var correct = 0
      var total = 0
      
      
      for (k <- 0 until lines.length) {
        val annotation = lines(k).split("\t")
        val word = annotation(0)
        val tag = annotation(1)
        var guess = hmmtags(k)
        //var guess = cslm.guess(word)
        
        // Check if word is punctuation
        if (word.matches("\\p{P}")) {
          guess = "Punct"
        }
        
        // Check if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
        var engClassification = engClassifier.classifyWithInlineXML(word)
        if (engClassification.contains('<')){
          guess = "Named Ent"
        }
                
        output.print(word + "," + guess + "," + tag)

        // Evaluate accuracy of model against words annotated as English or French in the gold standard
        if (tag =="Eng" || tag == "Fr" || tag == "Named Ent") {
          if (tag == "Eng" && guess == "Eng") {
            correct += 1
          }
          else if (tag == "Fr" && guess == "Fr"){
            correct += 1
          }
          else if (tag == "Named Ent" && guess == "Named Ent"){
            correct += 1
          }
          else{
            output.print("\t INCORRECT")           
          }
          total += 1

        }
        output.println()
        
      }      
      return correct.toDouble / total.toDouble    
    }
    

}



  



  



  
}