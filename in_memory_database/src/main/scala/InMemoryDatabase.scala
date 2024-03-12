import java.time.{Duration, Instant}
import scala.collection.{SeqOps, immutable, mutable}


class InMemoryDatabase(initialEpoch: Instant) {
  private type EpochDelta = Duration
  private type RecordKey = String
  private type FieldKey = String
  private type FieldAddress = (RecordKey, FieldKey)
  private type FieldValue = String
  private case class Field(value: String, expires: Option[EpochDelta])
  private type Record = Map[FieldKey, Field]
  private type Records = Map[RecordKey, Record]
  private type TTLQueue = immutable.TreeMap[EpochDelta, Vector[FieldAddress]]
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
    records: Records = Map.empty,
    ttlQueue: TTLQueue = immutable.TreeMap.empty
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
  def set(key: RecordKey, field: FieldKey, value: FieldValue, ttl: Option[TTLSpec]): Unit = {
    // Ex: Database epoch is 1000, now is 2000, lifetime is 100 => epochDelta should be 1100
    val expiryDelta: Option[EpochDelta] = ttl.map((now, lifetime) => Duration.between(database.epoch, now).plus(lifetime))
    // We must get the previous record to see if the old TTL is longer than the current one.
    val newValue =
      (database.records.get(key), expiryDelta) match {
        case (Some(Field(value, Some(previousEx))), Some(newEx)) if previousEx.compareTo(newEx) > 0 => Field(value, Some(previousEx))
        case _ => Field(value, expiryDelta)
      }
    // If a TTL is called for, insert the expiry record into the TTL queue.
    val newTTLqueue =
      expiryDelta match
        case Some(delta) =>
          val addressesAtDelta = database.ttlQueue.getOrElse(delta, Vector.empty)
          val newAddressesAtDelta = addressesAtDelta :+ (key, field)
          database.ttlQueue + (delta -> newAddressesAtDelta)
        case _ =>
          database.ttlQueue
    val newRecord = database.records.getOrElse(key, Map.empty) + (field -> newValue)
    val newRecords = database.records + (key -> newRecord)
    database = database.copy(records = newRecords, ttlQueue = newTTLqueue)
  }

  /**
   * Get a field value.  If the value has never been set or has expired, return None.
   * @param key The record key to retrieve
   * @param field The field key to retrieve
   * @return None if the value doesn't exist, otherwise the value.
   */
  def get(key: RecordKey, field: FieldKey): Option[FieldValue] =
    database.records
      .getOrElse(key, Map.empty)
      .get(field)
      .map(_.value)

  /**
   * Purge field if TTL has expired. Only purge if field has TTL set. The queuedDelta should already be in the epoch reference frame.
   * @param key The record key to purge
   * @param field The field name to purge.
   * @param queuedDelta The expected TTL delta. The field is only purged if its current delta is less than this delta.
   *                    (If it's not, it means the TTL queue record has be superseded by a subsequent `set`)
   */
  private def purge(key: RecordKey, field: FieldKey, queuedDelta: EpochDelta): Boolean = {
    database.records.get(key) match {
      case None => false
      case Some(record) =>
        record.get(field) match {
          case Some(Field(_, Some(expiryDelta))) if expiryDelta.compareTo(queuedDelta) <= 0 =>
            val newRecords = (record - field) match {
              case newRecord if newRecord.isEmpty => database.records - key
              case newRecord => database.records + (key -> newRecord)
            }
            database = database.copy(records = newRecords)
            true
          case _ => false
        }
    }
  }

  /**
   * Delete key unconditionally. If the resulting record is empty, remove the record.
   * @param key The record key
   * @param field The field name
   * @return `true` if the key existed and was deleted, else `false`
   */
  def delete(key: RecordKey, field: FieldKey): Boolean =
    database.records.get(key) match {
      case None => false
      case Some(record) =>
        record.get(field) match {
          case None => false
          case Some(Field(_, _)) =>
            val newRecords = (record - field) match {
              case newRecord if newRecord.isEmpty => database.records - key
              case newRecord => database.records + (key -> newRecord)
            }
            database = database.copy(records = newRecords)
            true
        }
    }

  /**
   * Return the fields and values for a requested record.
   * @param key the record key to retrieve
   * @return A sequence of the field names and values, sorted by field name. An empty list if the record doesn't exist.
   */
  def scan(key:RecordKey): Seq[(FieldKey, FieldValue)] =
    database.records
      .getOrElse(key, Map.empty)
      .toSeq
      .sortBy(_._1)
      .map((k, v) => k -> v.value)
  
  /**
   * Return fields and values for a requested record, where the field names have a given prefix
   * @param key The record key to retrieve
   * @param prefix The prefix to match against field names
   * @return A sequence of field names and values, sorted by field name. An empty list if the record doesn't exist
   *         or no fields match the prefix.
   */
  def scanPrefixed(key: RecordKey, prefix: FieldKey): Seq[(FieldKey, FieldValue)] =
    database.records
      .getOrElse(key, Map.empty)
      .filter((k, v) => k.startsWith(prefix))
      .toSeq
      .sortBy(_._1)
      .map((k, v) => k -> v.value)

  /**
   * Captures a copy of the database as of the current time.
   * @param now The current clock time.
   * @return A count of the number of fields backed up.
   */
  def backup(now: Instant): Int =
    backups = backups + (now -> database)
    database.records.map((key, fields) => fields.size).sum

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
    // because the ttlQueue is a TreeMap, we can efficiently partition it at a given point
    val (fieldsExpired, fieldsUnexpired) = database.ttlQueue.partition { (queuedDelta, _) => queuedDelta.compareTo(currentDelta) < 0 }
    var count: Int = 0
    for {
      (queuedDelta, addresses) <- fieldsExpired
      (key, field) <- addresses
    } {
      if(purge(key, field, queuedDelta)) {
        count = count + 1
      }
    }
    database = database.copy(ttlQueue = fieldsUnexpired)
    count
  }
}


