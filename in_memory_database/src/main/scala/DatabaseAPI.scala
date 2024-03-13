import java.time.{Duration, Instant}

class DatabaseAPI(db: InMemoryDatabase) {
  extension (thisString: String) {
    def toInstant: Instant = Instant.parse(thisString)
    def toDuration: Duration = Duration.parse(thisString)
  }

  def runQueries(queries: Seq[Array[String]]): Seq[String] = {
    for {
      query <- queries
    } yield {
      query match {
        case Array("SET", key, field, value) =>
          db.set((key, field), value, None)
          ""

        case Array("SET_AT", key, field, value, ts) =>
          val now = ts.toInstant
          db.advanceTimestamp(now)
          db.set((key, field), value, None)
          ""

        case Array("SET_AT_TTL", key, field, value, ttl, ts) =>
          val now = ts.toInstant
          db.advanceTimestamp(now)
          db.set((key, field), value, Some(now, ttl.toDuration))
          ""

        case Array("GET", key, field) =>
          db.get((key, field)).getOrElse("")

        case Array("GET_AT", key, field, ts) =>
          db.advanceTimestamp(ts.toInstant)
          db.get((key, field)).getOrElse("")

        case Array("DELETE", key, field) =>
          db.delete((key, field)).toString.toUpperCase

        case Array("DELETE_AT", key, field, ts) =>
          db.advanceTimestamp(ts.toInstant)
          db.delete((key, field)).toString.toUpperCase

        case Array("SCAN", key) =>
          db.scan(key)
            .map((k, v) => s"${k}(${v})")
            .mkString(", ")

        case Array("SCAN_AT", key, ts) =>
          db.advanceTimestamp(ts.toInstant)
          db.scan(key)
            .map((k, v) => s"${k}(${v})")
            .mkString(", ")

        case Array("SCAN_PREFIX", key, prefix) =>
          db.scanPrefixed(key, prefix)
            .map((k, v) => s"${k}(${v})")
            .mkString(", ")

        case Array("SCAN_PREFIX_AT", key, prefix, ts) =>
          db.advanceTimestamp(ts.toInstant)
          db.scanPrefixed(key, prefix)
            .map((k, v) => s"${k}(${v})")
            .mkString(", ")

        case Array("BACKUP_AT", ts) =>
          val now = ts.toInstant
          db.advanceTimestamp(now)
          val count = db.backup(now)
          s"$count"

        case Array("RESTORE_AT", fromTs, ts) =>
          val restorePoint = fromTs.toInstant
          val now = ts.toInstant
          db.restore(now, restorePoint) match
            case Some(value) => value.toString
            case None => ""
      }
    }
  }
}
