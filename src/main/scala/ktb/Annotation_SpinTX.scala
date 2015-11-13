package ktb

import scala.io.Source
import java.io._
import ktb.CharacterNGramModel._
import edu.stanford.nlp.ie.AbstractSequenceClassifier;
import edu.stanford.nlp.ie.crf._;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations;

object Annotation_SpinTX {
  
 def toWords(lines: Vector[String]) = lines flatMap { line =>
    line.replaceAll("\\t|\\n|\\r|\\p{P}", "").split(" ").filter(w => !w.isEmpty()) map (_.toLowerCase)
  }
  
  
  def main(args: Array[String]): Unit = {
    
    val testCorpus = "SpinTX/99SpinTXcorpus.txt"
    
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

    val annotator = new Annotator(hmm)
    annotator.annotate(testCorpus)
    
  }

}