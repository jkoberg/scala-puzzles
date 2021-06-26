package us.jkk.examples.laminar

import com.raquo.laminar.api.L._
import org.scalajs.dom

object Main {

  val inputCaption = span("First & last name:")
  val inputMods = Seq(typ := "text", defaultValue := "Me")

  div(
    h1("Hello world", color := "red"),
    inputCaption,
    input(inputMods, name := "fullName"),
    div(
      ">>",
      button("Submit"),
      "<<"
    )
  )

}
