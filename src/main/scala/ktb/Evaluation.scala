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
    
    val testCorpus = "KillerCronicas_clipped"
    val goldStandard = "KillerCronicasGoldStandard.txt"
    
    // Train two n-gram models on english and mexican oral corpora
    val n = 5 
    val engData = toWords(Source.fromFile("EngCorpus-1m.txt").getLines().toVector)
    val spanData = toWords(Source.fromFile("MexCorpus.txt").getLines().toVector)
    val enModel = new NgramModel("Eng", getConditionalCounts(engData, n), n)
    val esModel = new NgramModel("Spn", getConditionalCounts(spanData, n), n)
    
    // Build code switched language model from English and Spanish n-gram models
    val cslm = new codeSwitchedLanguageModel(Vector(enModel, esModel))
  
    // Build Hidden Markov Model with given transition probabilities (emission probabilities given by code switched language model)
    val testWords = Source.fromFile(testCorpus).getLines().flatMap(_.split(" ")).filter(w => !w.isEmpty()).toArray
    val tags = Array("Eng", "Spn")
    val transitions = (tags zip Array(tags zip Array(0.87, 0.13) toMap, tags zip Array(0.13, 0.87) toMap)) toMap
    val hmm = new HiddenMarkovModel(testWords, tags, transitions, cslm)
   
    // Build evaluator using code switched language model and hidden markov model
    val eval = new Evaluator(cslm, hmm)
    val accuracy = eval.evaluate(testCorpus, goldStandard)
    println(accuracy)

  }


}

class Evaluator(cslm : codeSwitchedLanguageModel, hmm: HiddenMarkovModel) {
  
    private[this] val classifier = CRFClassifier.getClassifier("classifiers/english.nowiki.3class.distsim.crf.ser.gz");
  
    
    /*
     * KC accuracy without HMM model, only guessing using clsm
     *  - Eng/Spn : 0.9540414161656646
     * KC accuracy with HMM params { p(same language) = 87%, p(code switch) = 13% }:
     *  - Eng/Spn : 0.9728790915163661
     *  - Eng/Spn/NamedEnt: 0.9445976435196791
     *  - Eng/Spn/NamedEnt/NonStSpn/NonStEng: 0.9409414408790111
     */
    def evaluate(text: String, goldStandard: String) : Double = {
      
      var outputFile = new FileOutputStream(text + "-output.txt")
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
        var classification = classifier.classifyWithInlineXML(word)
        if (classification.contains('<')){
          guess = "NamedEnt"
        }
                
        output.print(word + "," + guess + "," + tag)

        // Evaluate accuracy of model against words annotated as English or Spanish in the gold standard
        if (tag == "Eng" || tag == "Spn") {
          if (guess == tag)
            correct += 1
          else
            output.print("\t INCORRECT")
          total += 1

        }
        output.println()

      }
      
      return correct.toDouble / total.toDouble
      
    }
 
}



	



	


