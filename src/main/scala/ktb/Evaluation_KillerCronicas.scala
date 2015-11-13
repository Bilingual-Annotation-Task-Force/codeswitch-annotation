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
    
    //val testCorpus = "KillerCronicas_clipped"
    val testCorpus = "KillerCronicas/Killer_Cronicas"
    val goldStandard = "KillerCronicas/KillerCronicasGoldStandard"
    
    // Train two n-gram models on English and Spanish oral corpora
    val n = 5 
    val engData = toWords(Source.fromFile("TrainingCorpora/EngCorpus-1m.txt").getLines().toVector)
    val spanData = toWords(Source.fromFile("TrainingCorpora/MexCorpus.txt").getLines().toVector)
    val enModel = new NgramModel("Eng", getConditionalCounts(engData, n), n)
    val esModel = new NgramModel("Spn", getConditionalCounts(spanData, n), n)
    
    // Build code-switched language model from English and Spanish n-gram models
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, esModel))
  
    // Build Hidden Markov Model with estimated transition probabilities (emission probabilities given by code-switched language model)
    val testWords = Source.fromFile(testCorpus).getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).toArray
    // to do: grab tabs from cslm languages
    val tags = Array("Eng", "Spn")
    val transitions = (tags zip Array(tags zip Array(0.87, 0.13) toMap, tags zip Array(0.13, 0.87) toMap)) toMap
    val hmm = new HiddenMarkovModel(testWords, tags, transitions, cslm)
   
    // Build evaluator using code switched language model and hidden markov model
    val eval = new Evaluator(cslm, hmm)
    eval.annotate(testCorpus)
    //val accuracy = eval.evaluate(goldStandard)
    //println(accuracy)

  }


}

class Evaluator(cslm : codeSwitchedLanguageModel, hmm: HiddenMarkovModel) {
  
    private[this] val engClassifier = CRFClassifier.getClassifier("classifiers/english.nowiki.3class.distsim.crf.ser.gz");
    private[this] val spnClassifier = CRFClassifier.getClassifier("classifiers/spanish.ancora.distsim.s512.crf.ser.gz");

    
    /*
     * Killer Cronicas accuracy without HMM model, only guessing language using n-gram models:
     *  - Eng/Spn : 0.9540414161656646
     * Killer Cronicas accuracy with incorporated HMM model:
     *  - Eng/Spn : 0.9728790915163661
     *  - Eng/Spn/NamedEnt: 0.9445976435196791
     *  - Eng/Spn/NamedEnt/NonStSpn/NonStEng: 0.9409414408790111
     *  
     *  Killer Cronicas accuracy after adding Spanish classifier:
     *  - Eng/Spn/NamedEnt: 0.9496114314364502
     *  - Eng/Spn/NamedEnt/NonStSpn/NonStEng: 0.9459358222000249
     *  
     *  accuracy over all language tags and named entities (classifying any subtypes of spanish/english as spanish/english = correct)
     *  0.9459196102314251
     *  
     *  accuracy over only language tags, not tagging or evaluating named entities (classifying any subtypes of spanish/english as spanish/english = correct)
     *  0.9746654540730154
     */
    def evaluate(goldStandard: String) : Double = {
      
      var outputFile = new FileOutputStream(goldStandard + "_output.txt")
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
        var spnClassification = spnClassifier.classifyWithInlineXML(word)
        if (engClassification.contains('<') || spnClassification.contains('<')){
          guess = "NamedEnt"
        }
                
        output.print(word + "," + guess + "," + tag)

        // Evaluate accuracy of model against words annotated as English or Spanish in the gold standard
        if (tag == "Eng" || tag == "NonStEng" || tag == "EngNoSpace" || tag == "Spn" || tag == "NonStSpn" || tag == "SpnNoSpace" || tag == "NamedEnt") {
          if ((tag == "Eng" || tag == "NonStEng" || tag == "EngNoSpace") && (guess == "Eng")) 
              correct += 1
          else if ((tag == "Spn" || tag == "NonStSpn" || tag == "SpnNoSpace") && (guess == "Spn"))   
              correct += 1
          else if (tag == "NamedEnt" && guess == "NamedEnt")
              correct += 1
          else
              output.print("\t INCORRECT") 
          total += 1
        }
        output.println()
      }      
      return correct.toDouble / total.toDouble    
    }
    
    def annotate(filename: String) = {

      var output = new PrintStream(new FileOutputStream(filename + "-output.txt"))
      output.println("Token,Tag")
      val hmmtags = hmm.generateTags()
      val words = Source.fromFile(filename).getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).toArray
      
      for (k <- 0 until words.length) {
        var guess = hmmtags(k)
        var word = words(k)
        //var guess = cslm.guess(word)
        // Check if word is punctuation
        if (word.matches("[^a-zA-Z]")) {
          guess = "Punct"
        }
        
        // Check if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
        var engClassification = engClassifier.classifyWithInlineXML(word)
        var spnClassification = spnClassifier.classifyWithInlineXML(word)
        if (engClassification.contains('<') || spnClassification.contains('<')){
          guess = "NamedEnt"
        }
        
        output.println(word + "," + guess)

      }

    }
    

}



	



	


