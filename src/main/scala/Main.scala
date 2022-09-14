import scala.io.Source

object Application {
  def main(args: Array[String]): Unit = {

    // val startWord:String = args(0)
    // val endWord:String = args(0)
    val startWord:String = "cat"
    val endWord:String = "dog"
    // list of words is taken from http://www.gwicks.net/dictionaries.htm
    val dictionaryPath = "resources/english3.txt"

    if (startWord.length != endWord.length){
      println("start word and end word must be same length")
    } 
    else {
     // implicit workDict is to avoid threading the workDict through every subsequent function call. essentially creating a state monad
     implicit val workDict:List[String] = createworkDict(dictionaryPath, startWord.length()) //set is not the matmatical meaning
     val res = wordChain(Set(startWord), "", endWord, List(), workDict)
     
     println(s"\ndone!\nThe length of the chain is: ${res.length}\nThe chain is:\n")
     for {l <- res.reverse} println(l)
    }
  }

  def wordChain (wordQueue:Set[String], prevStep:String, endWord:String, acc:List[List[String]],
   workDict:List[String]): List[List[String]] = {
    val w = wordQueue.head
    val newAcc = List(prevStep, w) :: acc
    val nextWords = oneStep((w), workDict).toSet
    // println()
    // println(s"new call. head: ${w}")
    // println(s"newAcc: ${newAcc}")
    // println(s"tail: ${wordQueue.tail}")
    // println(s"newSteps: ${nextWords}")
    // println()
    if (nextWords.contains(endWord)){
      List(w,endWord) :: newAcc
    }
    else {
      val newCurWords = (nextWords union wordQueue.tail) -- newAcc.flatten.toSet  //important to remove already tried steps as it can otherwise flip the same letter back and forth
      wordChain(newCurWords, w, endWord, newAcc, workDict)
    
    }
  }

  def flipLetter (word:String, flipIdx:Int, workDict:List[String]) :List[String] = {
    val alphabet = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
    val flipChar = (w:String, i:Int, c:Char) => w.substring(0,i) + c + w.substring(i+1)
    val f = (c:Char) => if (c == word(flipIdx)) word else flipChar(word, flipIdx, c)
    val lst = alphabet.map(f)
    lst.filter((w:String) => w != word && workDict.contains(w) )
  }

  def oneStep(word:String, workDict:List[String]) :List[String] = {
    val idxs:List[Int] = (for {i <- 0 to word.length -1 } yield i).toList
    idxs.flatMap(flipLetter(word, _, workDict))
  }

  // function to trim the dictionary to relevent words in efforts to speed up lookups.
  def createworkDict(filename:String, wordLength:Int): List[String] = {
    val dict = Source.fromFile(filename).getLines.toList
    dict.filter( (x:String) => x.length == wordLength )
  }

}





/*    NOTES: 
Im trying to do a BFS, but its important to improve speed and complexity 
that the set of next words to take a step from does not contain dulicate 
and previous steps as this would essentially create a cycle in the search 
tree and can cause an infinite loop

*/