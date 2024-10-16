object DecodeTheMessage {

  def decodeMessage(key: String, message: String): String = {
    val keySeen = collection.mutable.Set.empty[Char]

    val keyWithoutDupes =
      for {
        c <- key
        if c != ' '
        if !keySeen.contains(c)
      } yield {
        keySeen.add(c)
        c
      }

    val table = keyWithoutDupes.zipWithIndex.map {
      case (c, i) => c -> (i + 'a').toChar
    }.toMap + (' ' -> ' ')

    String(message.map(table))
  }


}
