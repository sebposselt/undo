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
     // implicit workset is to avoid threading the workset through every subsequent function call. essentially creating a state monad
     implicit val workSet:List[String] = createWorkSet(dictionaryPath, startWord.length()) //set is not the matmatical meaning
     val res = wordChain(Set(startWord), endWord, List(), workSet)
     
     println(s"\ndone!\nThe length of the chain is: ${res.length}\nThe chain is:\n")
     for {w <- res.reverse} println(w)
    }
  }

  def wordChain (currentWords:Set[String], endWord:String, acc:List[String], workSet:List[String]): List[String] = {
    println(currentWords.count(s=>true))
    val w = currentWords.head
    val tail = currentWords.tail
    val newAcc = w :: acc
    val nextWords = oneStep((w), workSet).toSet
    
    if (nextWords.contains(endWord)){
      endWord::newAcc
    }
    else {
      val newCurWords = (nextWords union tail) -- newAcc.toSet
      
      wordChain(newCurWords, endWord, newAcc, workSet)
    }
  }

  def flipLetter (word:String, flipIdx:Int, workset:List[String]) :List[String] = {
    val alphabet = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
    val flipChar = (w:String, i:Int, c:Char) => w.substring(0,i) + c + w.substring(i+1)
    val f = (c:Char) => if (c == word(flipIdx)) word else flipChar(word, flipIdx, c)
    val lst = alphabet.map(f)
    lst.filter((w:String) => w != word && workset.contains(w) )
  }

  def oneStep(word:String, workset:List[String]) :List[String] = {
    val idxs:List[Int] = (for {i <- 0 to word.length -1 } yield i).toList
    idxs.flatMap(flipLetter(word, _, workset))
  }

  // function to trim the dictionary to relevent words in efforts to speed up lookups.
  def createWorkSet(filename:String, wordLength:Int): List[String] = {
    val dict = Source.fromFile(filename).getLines.toList
    dict.filter( (x:String) => x.length == wordLength )
  }

}
