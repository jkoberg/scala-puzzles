import java.time.{Duration, Instant}

object Main {
  val testQueries = Seq(
    Seq(
      Array("SET_AT_TTL", "customers/1", "name", "Joe Koberg", "PT5S", "2000-01-01T00:00:01Z"),
    ),
    Seq(
      Array("BACKUP_AT", "2000-01-01T00:00:02Z"),
      Array("SET_AT_TTL", "customers/2", "name", "Jeff Hicks", "PT20S", "2000-01-01T00:00:03Z"),
      Array("SET_AT_TTL", "customers/2", "name", "Jeff Hicks", "PT30S", "2000-01-01T00:00:04Z"),
      Array("GET_AT", "customers/1", "name", "2000-01-01T00:00:05Z"),
    ),
    Seq(
      Array("GET_AT", "customers/1", "name", "2000-01-01T00:00:10Z"),
      Array("GET_AT", "blah", "name", "2000-01-01T00:00:25Z"),
      Array("RESTORE_AT", "2000-01-01T00:00:02Z", "2020-01-01T00:00:00Z"),
      Array("GET_AT", "customers/1", "name", "2020-01-01T00:00:03Z"),
      Array("SET", "customers/3", "address/street", "123 Main St"),
      Array("SET", "customers/3", "address/city", "Townville"),
      Array("SET", "customers/3", "address/state", "KN"),
      Array("SET", "customers/3", "address/zip", "93939"),
      Array("SCAN_PREFIX", "customers/3", "address"),
    )
  )
  
  def main(args: Array[String]): Unit = {
    val db = InMemoryDatabase(Instant.parse("2000-01-01T00:00:00Z"))
    val api = DatabaseAPI(db)
    for {
      queries <- testQueries
    } {
      println(api.runQueries(queries).mkString("\n"))
      db.printState()
    }


    val db2 = InMemoryDatabase(Instant.now)
    speedTestLoop(db2)
    val t0 = System.nanoTime()
    speedTestLoop(db2, 10_000_000)
    val t1 = System.nanoTime()
    val tdelta = (t1 - t0) / 1.0e6;
    println(s"$tdelta milliseconds for 10m iterations")
    // with 1m ttl (nothing expires): 19434.751 milliseconds for 10m iterations => 1.9uS per iteration
    // with 1s ttl: 18857.7833 milliseconds for 10m iterations

  }

  def speedTestLoop(db: InMemoryDatabase, start:Long = 0, count: Long = 10_000_000): Unit = {
    for {
      i <- start.until(start + count)
    } {
      val rk = i.toString
      val fk = "name"
      val fv = s"name$i"
      db.set((rk, fk), fv, Some((Instant.now, Duration.ofSeconds(1))))
    }
  }

}
