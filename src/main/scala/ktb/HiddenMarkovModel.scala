package ktb

class HiddenMarkovModel (words: Array[String], tagSet: Array[String], transitions: Map[String, Map[String, Double]], cslm: codeSwitchedLanguageModel) {

  private[this] val v = Array.ofDim[Node](words.length, tagSet.length)
  
  def generateTags() : Array[String] = {
    viterbi()
    retrace()
  }
  
  def em(ctx: String, word: String) : Double = {
    cslm.prob(ctx, word)
  }
  
  def tr(ctx: String, tag: String) : Double = {
    transitions.get(ctx).get(tag)
  }
  
  def viterbi() = {
    
      // init; equal probability of starting with either tag
      for (tag <- 0 until tagSet.length) {
        v(0)(tag) = new Node(math.log(0.5), tag) 
      }

      for (word <- 1 until words.length) {
        for (tag <- 0 until tagSet.length) {
          val transitionProbs = List.range(0, tagSet.length) map (x => new Node(v(word-1)(x).getProb + math.log(tr(tagSet(x), tagSet(tag))), x)) 
          val max = transitionProbs.reduceLeft((n1: Node, n2: Node) => if (n1.getProb > n2.getProb) n1 else n2) 
          val emissionProb = em(tagSet(tag), words(word))
          v(word)(tag) = new Node(math.log(emissionProb) + max.getProb, max.getPrev)
        }       
      }
    
  }
  
  def retrace() : Array[String] = {
    val tags = new Array[String](words.length)

    // find most probable final tag    
    val last = List.range(0, tagSet.length) reduceLeft((x: Int, y: Int) => if (v(words.length - 1)(x).getProb > v(words.length - 1)(y).getProb) x else y)
    tags(words.length - 1) = tagSet(last)
    
    // follow backpointers to most probable previous tags
    var prev = v(words.length - 1)(last).getPrev  
    for (k <- (words.length - 2) to 0 by -1){
      tags(k) = tagSet(prev)
      prev = v(k)(prev).getPrev  
    }
    return tags
  }
  
  class Node(prob: Double, prevTag: Int) {
      def getProb : Double = { prob }
      def getPrev : Int = { prevTag }

  }
      


  
}