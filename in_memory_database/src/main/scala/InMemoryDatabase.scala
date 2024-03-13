import java.time.{Duration, Instant}
import scala.collection.{SeqOps, immutable, mutable}


class InMemoryDatabase(initialEpoch: Instant) {
  private type EpochDelta = Duration
  private type RecordKey = String
  private type FieldKey = String
  private type CompositeKey = (RecordKey, FieldKey)
  private type FieldValue = String
  private case class Field(value: FieldValue, expires: Option[EpochDelta])
  private type KVStore = immutable.TreeMap[CompositeKey, Field]
  private type TTLQueue = immutable.TreeSet[(EpochDelta, CompositeKey)]
  private type TTLSpec = (Instant, Duration)

  /**
   * The database data structures, used to store a dynamic key-value store with TTL. All members
   * are persistent / immutable values, so the entire storage can be referenced in a backup without
   * ongoing mutations destroying its state.
   * @param epoch The point in time from which all TTLs are computed. All TTLs are set based on the offset from
   *              the current time to the epoch. When a new timestamp arrives, its offset from the epoch is also
   *              computed, and that offset is used to evaluate expired keys. When the database is restored, the epoch
   *              is set to the restoral time, effectively pushing all TTLs into the future.
   * @param records The collection of records and fields, keyed by record ID and field name.
   * @param ttlQueue An ordered mapping of keys to expire, by expiration time.  Since there can be more than one key
   *                 expiring at a given time, this is a list of field addresses.
   */
  private case class Storage(
    epoch: Instant,
    records: KVStore = immutable.TreeMap.empty,
    ttlQueue: TTLQueue = immutable.TreeSet.empty
  )

  private var database: Storage = Storage(epoch = initialEpoch)

  /**
   * The set of backups are simply copies of the database at given times.  A ordered map is used so we
   * can find the last backup before a given restore point.
   */
  private var backups: immutable.TreeMap[Instant, Storage] = immutable.TreeMap.empty

  def printState(): Unit = {
    println(s"Epoch: ${database.epoch}")
    println(s"Records: ${database.records}")
    println(s"TTLQueue: ${database.ttlQueue}")
  }

  /**
   * Set a field value.  If the record doesn't exist, create it.  If a TTL is specified, the record will remain
   * valid until the later of the given ttl and any previously set ttl.
   * If a previous TTL has been set, and a new set call for the same field doesn't specify a TTL, the record becomes permanent.
   * If a previous TTL has not been set, and a new call sets one, the record will expire at the new TTL
   * @param key The record key to create or update
   * @param field The field name to create or update
   * @param value The field value to set
   * @param ttl If None, sets a non-expiring value.  If given, sets the TTL based on the given current time and TTL duration.
   */
  def set(cKey: CompositeKey, value: FieldValue, ttl: Option[TTLSpec]): Unit = {
    // Ex: Database epoch is 1000, now is 2000, lifetime is 100 => epochDelta should be 1100
    val expiryDelta: Option[EpochDelta] = ttl.map((now, lifetime) => Duration.between(database.epoch, now).plus(lifetime))
    // We must get the previous record to see if the old TTL is longer than the current one.
    val newValue =
      (database.records.get(cKey), expiryDelta) match {
        case (Some(Field(value, Some(previousEx))), Some(newEx)) if previousEx.compareTo(newEx) > 0 => Field(value, Some(previousEx))
        case _ => Field(value, expiryDelta)
      }
    // If a TTL is called for, insert the expiry record into the TTL queue.
    val newTTLqueue =
      expiryDelta match
        case Some(delta) =>
          database.ttlQueue.incl((delta, cKey))
        case _ =>
          database.ttlQueue
    val newRecords = database.records + (cKey -> newValue)
    database = database.copy(records = newRecords, ttlQueue = newTTLqueue)
  }

  /**
   * Get a field value.  If the value has never been set or has expired, return None.
   * @param key The record key to retrieve
   * @param field The field key to retrieve
   * @return None if the value doesn't exist, otherwise the value.
   */
  def get(cKey: CompositeKey): Option[FieldValue] =
    database.records
      .get(cKey)
      .map(_.value)



  /**
   * Delete key unconditionally. If the resulting record is empty, remove the record.
   * @param key The record key
   * @param field The field name
   * @return `true` if the key existed and was deleted, else `false`
   */
  def delete(cKey: CompositeKey): Boolean =
    database.records.get(cKey) match {
      case None =>
        false
      case Some(Field(_, _)) =>
        database = database.copy(records = database.records - cKey)
        true
    }


  /**
   * Return the fields and values for a requested record.
   * @param key the record key to retrieve
   * @return A sequence of the field names and values, sorted by field name. An empty list if the record doesn't exist.
   */
  def scan(key:RecordKey): Iterable[(FieldKey, FieldValue)] =
    database.records
      .rangeFrom((key, ""))
      .takeWhile { case ((rk, fk), value) => rk == key }
      .map { case ((rk, fk), v)  => fk -> v.value }

  /**
   * Return fields and values for a requested record, where the field names have a given prefix
   * @param key The record key to retrieve
   * @param prefix The prefix to match against field names
   * @return A sequence of field names and values, sorted by field name. An empty list if the record doesn't exist
   *         or no fields match the prefix.
   */
  def scanPrefixed(key: RecordKey, prefix: FieldKey): Iterable[(FieldKey, FieldValue)] =
    database.records
      .rangeFrom((key, prefix))
      .takeWhile { case ((rk, fk), v) => fk.startsWith(prefix) }
      .map { case ((rk, fk), v) => fk -> v.value }

  /**
   * Captures a copy of the database as of the current time.
   * @param now The current clock time.
   * @return A count of the number of fields backed up.
   */
  def backup(now: Instant): Int =
    backups = backups + (now -> database)
    database.records.size

  /**
   * Restores the latest point-in-time backup of the database that occurred before the requested restore point.
   * When the database is restored, any TTLs that were current at the time of backup are moved forward to be
   * relative to the restoral time. (In other words, backup "freezes" the TTL timeline and restore "unfreezes" it).
   * If no backup exists before the given restore point, the current database is not changed.
   * @param now The current time.
   * @param restorePoint The point-in-time to restore from.  The latest backup before or equal to this time
   *                     will be used.
   * @return None if no backup was restore, otherwise Some(timestamp) indicating the timestamp of the backup. This
   *         timestamp will be before, or equal to, the requested restorePoint.
   */
  def restore(now: Instant, restorePoint: Instant): Option[Instant] =
    backups.rangeTo(restorePoint).lastOption match
      case Some((when, snapshot)) =>
        database = snapshot.copy(epoch = now)
        Some(when)
      case None =>
        None

  /**
   * Perform deletion of expired keys.
   * @param now the current time.
   * @return The number of fields expired.
   */
  def advanceTimestamp(now: Instant): Int = {
    // ex: DB Epoch is 1000. Queued delta is 1100. Now is 2500.
    //     => currentDelta is duration between 1000 and 2500 => 1500
    //     => queued delta is less than current delta => it's expired
    val currentDelta = Duration.between(database.epoch, now)
    val matchingTtlEntries = database.ttlQueue.takeWhile {
      case (queuedDelta, _) => queuedDelta.compareTo(currentDelta) <= 0
    }
    val matchingRecords = 
      for {
        (queuedDelta, cKey) <- matchingTtlEntries
        case Field(_, Some(expiryDelta)) <- database.records.get(cKey)
        if expiryDelta.compareTo(queuedDelta) <= 0
      } yield {
        cKey
      }
    database = database.copy(
      records = database.records.removedAll(matchingRecords), 
      ttlQueue = database.ttlQueue.removedAll(matchingTtlEntries)
    )
    matchingRecords.size
  }
}


