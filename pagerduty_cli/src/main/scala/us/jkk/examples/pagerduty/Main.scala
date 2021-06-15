package us.jkk.examples.pagerduty

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.util.ByteString
import play.api.libs.json.{JsError, JsSuccess, Json, Reads}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}

/**
 * Response schema for the PagerDuty JSON API
 */
object ApiModels {
  case class ContactMethod (summary: String, address: String)
  object ContactMethod { implicit val reads: Reads[ContactMethod] = Json.reads }

  case class User (name: String, contact_methods: Vector[ContactMethod])
  object User { implicit val reads: Reads[User] = Json.reads }

  case class ApiResponse (users: Option[Vector[User]])
  object ApiResponse { implicit val reads: Reads[ApiResponse] = Json.reads }
}


object Main extends App {

  import ApiModels._

  // Akka HTTP requires an ActorSystem and Future resolution requires an ExecutionContext
  implicit val system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "SingleRequest")
  implicit val executionContext: ExecutionContext = system.executionContext

  val apiRoot = Uri("https://api.pagerduty.com")

  val req = HttpRequest(
    uri = apiRoot
      .withPath(Uri.Path("/users"))
      .withQuery(Uri.Query("include[]" -> "contact_methods")),
    headers = Seq(
      RawHeader("Authorization", "Token token=y_NbAkKc66ryYTWUXYEu"),
      RawHeader("Content-Type", "application/json"),
      RawHeader("Accept", "application/vnd.pagerduty+json;version=2")
    )
  )

  val programF = Http().singleRequest(req)
    .flatMap {
      case HttpResponse(StatusCodes.OK, _, entity, _) =>
        entity.dataBytes.runFold(ByteString.empty)(_ ++ _)
      case res =>
        sys.error(s"Invalid response: ${res}")
    }
    .map { body =>
      Json.parse(body.toArray).validate[ApiResponse] match {
        case JsError(errors) =>
          println(s"ERROR: Unexpected response format: ${JsError(errors)}")
        case JsSuccess(apiResp, path) =>
          apiResp.users match {
            case None =>
              println("WARNING: User data not included in API response.")
            case Some(users) =>
              if(users.isEmpty) {
                println(s"No users found.")
              } else {
                users.foreach { u =>
                  if (u.contact_methods.isEmpty) {
                    println(s"User ${u.name} has no contact methods.")
                  } else {
                    println(s"User ${u.name} may be contacted at:")
                    u.contact_methods.foreach { m =>
                      println(s"    ${m.summary} @ ${m.address}")
                    }
                  }
                }
              }
          }
      }
    }
    .recover { exception =>
      println(s"ERROR: Exception thrown: ${exception}")
    }


  Await.result(programF, Duration.Inf)
  system.terminate()
}
