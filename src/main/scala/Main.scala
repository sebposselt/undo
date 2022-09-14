import scala.io.Source

object Application {
  def main(args: Array[String]): Unit = {

    val startWord:String = args(0)
    val endWord:String = args(1)
    // list of words is taken from http://www.gwicks.net/dictionaries.htm
    val dictionaryPath = "resources/english3.txt"

    if (startWord.length != endWord.length){
      println("start word and end word must be same length")
    }
    else {
      // implicit workDict is to avoid threading the workDict through every auxilery function call.
      implicit val workDict:List[String] = createworkDict(dictionaryPath, startWord.length()) //set is not the matmatical meaning
      val res = wordChain(Set(startWord), endWord, Set(), workDict)
      val chain = findChain(res,startWord,endWord,List(endWord))

      println(s"\ndone!\nThe length of the chain is: ${chain.length}\nThe chain is:\n")
      for {s <- chain} println(s)
    }
  }

  def findChain( steps: Set[List[String]], startWord:String, endWord:String, acc:List[String]): List[String] = {
    if (acc.contains(startWord)) {
      acc
    }
    else {
      val step = steps.find(l => l(1) == endWord)
      val (previous, steps2) = step match {
        case Some( lst ) =>
          (lst(0), steps - lst)
        case None =>
          ("this will never happen as I know find() will succeed", Set())
      }
      findChain(steps2, startWord, previous, previous::acc)
    }
  }

  def wordChain(wordQueue:Set[String],
                endWord:String,
                acc:Set[List[String]],
                workDict:List[String]): Set[List[String]] = {            
    // find all possible steps from every word in wordqueue
    val tmp = wordQueue.map( w => (w, oneStep(w,workDict)))
    // add the the steps to the accumulator
    val additionsToAcc = tmp.flatMap( (s,l) => l.map( s2 => List(s,s2) ) )
    val newAcc = acc ++ additionsToAcc
    
    // select the new possible steps
    val nextWords = (tmp.flatMap(tup => tup._2) )
    
    // base case
    if (nextWords.contains(endWord)){
      val x = tmp.find( (s,l) => l.contains(endWord))
      val y = x match {
        case Some( (str,_) ) =>
          str
        case None =>
          "this will never happen due to the if"
      }
      newAcc + List(y,endWord)
    }
    // recursive case
    else {
      wordChain(nextWords -- acc.flatten, endWord, newAcc, workDict)
    }
  }

  // aux-function to flip a single letter of a word to all possibilities and check that it yields an actual word
  def flipLetters (word:String, flipIdx:Int, workDict:List[String]) :List[String] = {
    val alphabet = List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
    val flipChar = (w:String, i:Int, c:Char) => w.substring(0,i) + c + w.substring(i+1)
    val f = (c:Char) => if (c == word(flipIdx)) word else flipChar(word, flipIdx, c)
    val lst = alphabet.map(f)
    lst.filter((w:String) => w != word && workDict.contains(w) )
  }

  // calcualte all possible steps form a given word
  def oneStep(word:String, workDict:List[String]) :List[String] = {
    val idxs:List[Int] = (for {i <- 0 to word.length -1 } yield i).toList
    idxs.flatMap(flipLetters(word, _, workDict))
  }

  // function to trim the dictionary to relevent words in efforts to speed up lookups.
  def createworkDict(filename:String, wordLength:Int): List[String] = {
    val dict = Source.fromFile(filename).getLines.toList
    dict.filter( (x:String) => x.length == wordLength )
  }

}