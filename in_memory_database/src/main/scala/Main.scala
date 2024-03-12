import java.time.Instant

object Main {
  val testQueries = Seq(
    Array("SET_AT_TTL", "customers/1", "name", "Joe Koberg", "PT5S", "2000-01-01T00:00:01Z"),
    Array("BACKUP_AT", "2000-01-01T00:00:02Z"),
    Array("SET_AT_TTL", "customers/2", "name", "Jeff Hicks", "PT20S", "2000-01-01T00:00:03Z"),
    Array("SET_AT_TTL", "customers/2", "name", "Jeff Hicks", "PT30S", "2000-01-01T00:00:04Z"),
    Array("GET_AT", "customers/1", "name", "2000-01-01T00:00:05Z"),
    Array("GET_AT", "customers/1", "name", "2000-01-01T00:00:10Z"),
    Array("GET_AT", "blah", "name", "2000-01-01T00:00:25Z"),
    Array("RESTORE_AT", "2000-01-01T00:00:02Z", "2020-01-01T00:00:00Z"),
    Array("GET_AT", "customers/1", "name", "2020-01-01T00:00:03Z")
  )
  
  def main(args: Array[String]): Unit = {
    val db = InMemoryDatabase(Instant.parse("2000-01-01T00:00:00Z"))
    val api = DatabaseAPI(db)
    println(api.runQueries(testQueries).mkString("\n"))
    db.printState()
  }
}
