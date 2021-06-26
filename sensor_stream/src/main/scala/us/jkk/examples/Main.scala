package us.jkk.examples

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl._
import akka.http.scaladsl.model.StatusCodes
import akka.stream.Attributes
import akka.stream.Attributes.InputBuffer
import akka.stream.scaladsl._
import us.jkk.examples.Models.{SensorAverage, SensorBatch, SensorReading}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Models {
  case class SensorReading(
    millis: Long,
    value: Double
    )

  case class SensorBatch(
    nextWatermark: Long = 0,
    accumulated: Vector[SensorReading] = Vector.empty,
    pendingEmit: Option[Vector[SensorReading]] = None
  )

  case class SensorAverage(batch: Vector[SensorReading]) {
    def getAverageValue: Option[Double] = {
      if(batch.isEmpty) {
        None
      } else {
        val values = batch.map(_.value)
        Some(values.sum / values.length)
      }
    }
    def summaryText: String = s"Readings starting at ${batch.head.millis}, ending at ${batch.last.millis}, of ${batch.length} readings, with average ${getAverageValue}"
  }

}

object Main extends App {
  implicit val system = ActorSystem()
  import system.dispatcher

  val url = "ws://at-streaming-sim.azurewebsites.net/ws/simple/2083"

  val averageFlow = Flow[Message]
    .map {
      case TextMessage.Strict(txt) => txt
      case _ => sys.error("Streams or binary messages aren't handled")
    }
    .map { txt => txt.split(',')  match {
      case a if a.length == 2 => SensorReading(a(0).toLong, a(1).toDouble)
      case _ => sys.error(s"Malformed sensor reading ${txt}")
    }}
    .scan(SensorBatch()) {
      case (b:SensorBatch, r:SensorReading) =>
        if(b.nextWatermark == 0) {
          SensorBatch(r.millis + 5000, Vector(r), None)
        } else if(r.millis < b.nextWatermark) {
          SensorBatch(b.nextWatermark, b.accumulated :+ r, None)
        } else {
          SensorBatch(b.nextWatermark + 5000, Vector(r), Some(b.accumulated))
        }
    }
    .filter(_.pendingEmit.nonEmpty)
    .map(b => SensorAverage(b.pendingEmit.get))
    .map(_.summaryText)
    .toMat(Sink.foreach[Any](println))(Keep.right)

  // This endpoint doesn't require any messages to start - so provide an empty stream
  val emptySource = Source.never[Message]

  // Limit buffer size to 1 to ensure immediate handling or visibility of results
  val flow = Flow
    .fromSinkAndSourceMat(averageFlow, emptySource)(Keep.left)
    .withAttributes(Attributes(InputBuffer(1,1)))

  // Make the websocket connection and handle potential failure to upgrade from HTTP to WS
  val (upgradeResponse, whenClosed) = Http().singleWebSocketRequest(url, flow)

  val upgraded = upgradeResponse.map { upgrade =>
    if(upgrade.response.status == StatusCodes.SwitchingProtocols) {
      Done
    } else {
      throw new RuntimeException(s"Connection failed!")
    }
  }

  // Notify the user when the stream closes
  whenClosed.foreach { _ =>
    println("Stream closed")
  }

  Await.result(whenClosed, Duration.Inf)
  sys.exit(0)
}
