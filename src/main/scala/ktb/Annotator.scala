package ktb

import scala.io.Source
import java.io._
import ktb.CharacterNGramModel._
import edu.stanford.nlp.ie.AbstractSequenceClassifier;
import edu.stanford.nlp.ie.crf._;
import edu.stanford.nlp.io.IOUtils;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.ling.CoreAnnotations;


class Annotator (hmm: HiddenMarkovModel){
    private[this] val engClassifier = CRFClassifier.getClassifier("classifiers/english.nowiki.3class.distsim.crf.ser.gz");

    def annotate(filename: String) = {
      var output = new PrintStream(filename + "_annotated.txt", "UTF-8")
      output.println("Token,Tag")
      val hmmtags = hmm.generateTags()
      val words = hmm.getWords()
      for (k <- 0 until words.length) {
        var guess = hmmtags(k)
        var word = words(k)
        
        // Check if word is punctuation
        if (word.matches("\\p{P}")) {
          guess = "Punct"
        }
        
        // Check if word is a named entity, i.e., <PERSON>, <LOCATION>, etc. 
        var engClassification = engClassifier.classifyWithInlineXML(word)
        if (engClassification.contains('<')){
          guess = "NamedEnt"
        }
        
        output.println(word + "," + guess)
      }

    }
}