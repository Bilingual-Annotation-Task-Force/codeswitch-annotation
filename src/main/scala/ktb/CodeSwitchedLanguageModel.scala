package ktb

import scala.io.Source
import java.io._
import edu.stanford.nlp.ie.crf._
import ktb.CharacterNGramModel._


class codeSwitchedLanguageModel(
      models: Vector[NgramModel] ) {
    
    def guess(word: String) : String = {
      val highestProb = models.map(_.stringProb(word.toLowerCase)).max
      val guess = models.filter(x => x.stringProb(word.toLowerCase) == highestProb)
      guess(0).getLanguage()
    }
    
    def prob(language: String, word: String) : Double = {
      models.filter(x => x.getLanguage() == language)(0).stringProb(word.toLowerCase)
    }

    
}

