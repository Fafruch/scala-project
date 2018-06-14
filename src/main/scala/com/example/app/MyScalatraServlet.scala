package com.example.app

import org.scalatra._

class MyScalatraServlet extends ScalatraServlet {

  get("/:mode/:year") {
    val mode: String = params("mode")
    val year: Int = params("year").toInt

    if (year < 1950 || year > 2018) {
      Response.getErrorResponse(ErrorObject("400", "Wrong year parameter", Some("Only 1950 to 2018 years are available")))

    } else mode match {
        case "races" | "drivers" | "team" | "fastest-laps" => Response.getResponse(mode, year)
        case _ => Response.getErrorResponse(ErrorObject("404", "No route for '/" + mode + "'", None))
      }
  }
}
