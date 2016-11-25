package calculator


object TweetLength {
  final val MaxTweetLength = 140

  implicit def hex2int (hex: String): Int = Integer.parseInt(hex, 16)

  /**
    * This function takes a Signal[String] of the text of a Tweet being typed,
    * and returns a Signal[Int] with the corresponding number of characters
    * that are left.
    */
  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    Signal(MaxTweetLength - tweetLength(tweetText()))
  }

  /**
    * uses the signal of remaining char count to compute the signal of color
    * For better visual feedback, we also want to display the remaining character count in colors indicating how "safe" we are:
    * If there are 15 or more characters left, the color "green"
    * If there are between 0 and 14 characters left, included, the color "orange"
    * Otherwise (if the remaining count is negative), the color "red"
    */
  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {

    if (remainingCharsCount() < 0) Signal("red")
    else if (remainingCharsCount() >= 0 && remainingCharsCount() <= 14) Signal("orange")
    else Signal("green")
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should be simply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
