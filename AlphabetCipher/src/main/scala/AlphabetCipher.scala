import scala.collection.immutable.NumericRange

object AlphabetCipher {
  val alpha = 'a' to 'z'

  lazy val chart: Map[Char, Seq[String]] = {
    (for ((main, ctr) <- alpha.zipWithIndex) yield {
      val leftSide = (main to alpha(alpha.size - 1)).map(_.toString)
      val rightSide = if (ctr == 0) Nil else alpha(0) to alpha(ctr - 1)
      val res = (leftSide ++ rightSide).map(_.toString)
      main -> res.toVector
    }).toMap
  }

  def lookup(letter: Char, pos: Int) =
    (for (
      char <- chart.keys;
      if (chart(char).toSeq(pos).toString == letter.toString)
    ) yield char).head

  val getPos: Char => Int = c => alpha.indexOf(c)

  def encode(keyword: String, message: String): String = {
    val rptKey = keyword * ((message.length / keyword.length) + 1)
    val keywordSeq: Seq[Char] =
      rptKey.toArray.slice(0, message.length).toSeq

    val messageSeq = message.toSeq
    val encodedMessage =
      (for ((key, ctr) <- keywordSeq.zipWithIndex) yield {
        val ch = messageSeq(ctr)
        val keyPos = getPos(key);
        chart(ch)(keyPos)
      }).mkString("")

    encodedMessage
  }

  def decode(keyword: String, message: String): String = {
    val rptKey = keyword * ((message.length / keyword.length) + 1)
    val keywordSeq: Seq[Char] =
      rptKey.toArray.slice(0, message.length).toSeq

    val messageSeq = message.toSeq
    val decodedMessage =
      (for ((key, ctr) <- keywordSeq.zipWithIndex) yield {
        val keyPos = getPos(key);
        val ch = messageSeq(ctr)
        lookup(ch, keyPos)
      }).mkString("")

    decodedMessage
  }

  def figureOutKeyword(deciphered: String): List[String] = {
     (for (
        (char, i) <- deciphered.toSeq.zipWithIndex
      ) yield {
        val keyword = deciphered.toSeq.slice(0, i + 1).mkString("")
        val occurences = keyword.r.findAllMatchIn(deciphered).length

        if (occurences > 1) {
          Some(keyword)
        } else {
          None
        }
      }).toList.flatten
  }

  def decipher(cipher: String, message: String): String = {
    val deciphered = (for ((char, i) <- message.toSeq.zipWithIndex) yield {
      val cipherChar: String = cipher.toSeq(i).toString
      val idx = chart(char).toSeq.indexOf(cipherChar)
      alpha.toSeq(idx)
    }).mkString

    val prefix = deciphered.take(2)
    val firstOccurance = deciphered.split(prefix)
    val finalKeyword = prefix + firstOccurance(1)

    finalKeyword
  }

}
