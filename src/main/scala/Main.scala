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

      val res = wordChain(Set(startWord), endWord, Set(), workDict)
      println("steps done")
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
      println(step)
      val (steps2,previous) = step match {
        case Some( lst ) =>
          (steps - lst, lst(0))
        case None =>
          "this will never happen as I know find succedes"
      }
      findChain(steps2, startWord, previous, previous::acc)
    }
  }


  // def wordChain ( wordQueue:Set[String],
  //                 prevStep:String,
  //                 endWord:String,
  //                 acc:List[List[String]],
  //                 workDict:List[String]): List[List[String]] = {
  //   val w = wordQueue.head
  //   val newAcc = List(prevStep, w) :: acc
  //   // val nextWords = oneStepSmart((w), endWord, workDict).toSet
  //   // val nextWords = oneStep((w), workDict).toSet


  //   // println()
  //   // println(s"new call. head: ${w}")
  //   // println(s"newAcc: ${newAcc}")
  //   // println(s"tail: ${wordQueue.tail}")
  //   // println(s"newSteps: ${nextWords}")
  //   // println()
  //   if (nextWords.contains(endWord)){
  //     List(w,endWord) :: newAcc
  //   }
  //   else {
  //     // val newCurWords = ((nextWords - (w)) union wordQueue.tail)  //important to remove already tried steps as it can otherwise flip the same letter back and forth
  //     val newCurWords = (nextWords union wordQueue.tail) -- newAcc.flatten.toSet  //important to remove already tried steps as it can otherwise flip the same letter back and forth
  //     wordChain(newCurWords, w, endWord, newAcc, workDict)

  //   }
  // }

  def wordChain(wordQueue:Set[String],
                endWord:String,
                acc:Set[List[String]],
                workDict:List[String]): Set[List[String]] = {
    val tmp = wordQueue.map( w => (w, oneStep(w,workDict)))
    val additionsToAcc = tmp.flatMap( (s,l) => l.map( s2 => List(s,s2) ) )
    val newAcc = acc ++ additionsToAcc
    val nextWords = tmp.flatMap(tup => tup._2) 
    
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
    else {
      wordChain(nextWords, endWord, newAcc, workDict)
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


  // if one char is correctly placed in word it will not try to flip it. this is greedy and the problem may not always be solvable by a greedy solution
  def oneStepSmart(word:String, endWord:String, workDict:List[String]) :List[String] = {
    val idxs:List[Int] = (for {i <- 0 to word.length -1 } yield i).toList
    val indx2:List[Int] = idxs.filter( i => word(i) != endWord(i) )
    indx2.flatMap(flipLetter(word, _, workDict))
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