import scala.collection.mutable

/**
 * Implement a solver for the adversarial "missing letter" (or hangman) game
 *
 * * The adversary presents a lexicon and array of blanks to the player.
 * * The player attempts to guess a letter that will fill in a blank
 * * If possible, the adversary "secretly" chooses another target word on each turn, which doesn't contain
 *   the player's guess - producing a "miss"
 * * The player would ideally make guesses that reduce the adversary's ability to swap words out and produce
 *   misses.
 *
 * This is a depth-first minimax strategy, where the number of generated misses are counted, the player tries
 * to minimize them, and the adversary tries to maximize them.
 */
object MissingLetterGame {

  val BLANK = '_'

  case class BoardState(partialWord: String, misses: Set[Char] = Set.empty)


  /**
   * Given a board state and lexicon, return words that could be playable.
   */
  def filterWordsByBoardState(boardState: BoardState, words: Seq[String]): (mutable.Set[String], mutable.Set[Char]) = {
    val (filledPositions, blankPositions) = boardState.partialWord.zipWithIndex.partition((c, i) => c != BLANK)
    val guesses = boardState.misses ++ filledPositions.map((c, i) => boardState.partialWord.charAt(i))
    val possibleWords = mutable.Set.empty[String]
    val possibleGuesses = mutable.Set.empty[Char]
    for {
      word <- words
      // Word must be the same length as the target
      if word.length == boardState.partialWord.length
      // Word must match already guessed letter positions
      if filledPositions.forall((c, i) => word.charAt(i) == c)
      // Word must not contain missed guesses
      if !boardState.misses.exists(c => word.contains(c))
      // Word must not have an already-guessed character in a different position
      if blankPositions.forall((c, i) => !guesses.contains(word.charAt(i)))
    } {
      possibleWords.add(word)
      possibleGuesses.addAll(blankPositions.map((c, i) => word.charAt(i)))
    }
    (possibleWords, possibleGuesses)
  }


  /** Given a partially filled word, chosen word, and guessed character, update the partial
   * word to fill in the appropriate blanks with the guess */
  def replaceBlanks(partialWord:String, chosenWord: String, guess: Char): String = {
    val partialChars = partialWord.toCharArray
    for {
      i <- chosenWord.indices
      if chosenWord.charAt(i) == guess
    } {
      partialChars(i) = guess
    }
    String(partialChars)
  }
  


  /** Given a board state and set of words, return the guess that minimizes the number of misses
   * that the adversary can produce.
   *
   * Use memoized depth-first traversal, keyed on the board state and set of misses.
   * */
  def traverse(
    state: BoardState,
    words: Seq[String],
    lastGuess: Option[Char],
    memo: mutable.Map[BoardState, (Int, Option[Char])] = mutable.HashMap.empty,
  ): (Int, Option[Char]) = {
    memo.getOrElseUpdate(state, {
      // Player's turn - find a guess that leads to minimum misses
      val (possibleWords, possibleGuesses) = filterWordsByBoardState(state, words)
      possibleGuesses match {
        // If no guess is possible (no words must have been possible), the game is over - return the count of misses.
        case guesses if guesses.isEmpty =>
          (state.misses.size, lastGuess)

        // Otherwise, try each guess and find the best.
        case guesses =>
          guesses.map { guess =>
            // Player has chosen guess - Adversary's turn
            possibleWords.partition(w => w.contains(guess)) match {
              // If this guess can yield a miss, produce the miss.
              case (_, missWords) if missWords.nonEmpty =>
                val newState = state.copy(misses = state.misses + guess)
                traverse(newState, missWords.toSeq, Some(guess), memo)

              // If there is only one word left, there will be no more misses, terminate by returning the current count.
              case (Seq(hit), _) =>
                (state.misses.size, Some(guess))

              // Player's guess could result in more than one hit, choose the one that maximizes
              // future misses and go forward with it.
              case (hitWords, _) =>
                hitWords.map { hit =>
                  val newState = state.copy(partialWord = replaceBlanks(state.partialWord, hit, guess))
                  traverse(newState, hitWords.toSeq, Some(guess), memo)
                }.max
            }
          }.min
      }
    })
  }



  def bestGuess(state:BoardState, words: Seq[String]): Char = {
    traverse(state, words, None)._2.get
  }


}
