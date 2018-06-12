package com.example.app

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import scala.collection.mutable.ListBuffer

import org.scalatra._

import scala.collection.mutable.ListBuffer

sealed trait ResponseTrait

case class ErrorObject(status: String, title: String, details: Option[String])
case class ErrorResponse(errors: ListBuffer[ErrorObject]) extends ResponseTrait

case class RaceResult(grandPrix: String, date: String, winner: String, car: String, laps: Int, time: String)
case class RacesResponse(data: ListBuffer[Option[RaceResult]] = ListBuffer()) extends ResponseTrait

case class DriverStanding(pos: Int, driver: String, nationality: String, car: String, pts: Int)
case class DriversResponse(data: ListBuffer[Option[DriverStanding]] = ListBuffer()) extends ResponseTrait


object Response {
  def getURL(mode: String, year: Int): String = {
    val baseUrl = "https://www.formula1.com/en/results.html/"

    baseUrl + "/" + year + "/" + mode + ".html"
  }

  def getResponse(mode: String, year: Int): String = {
    val url = getURL(mode, year)

    val browser = JsoupBrowser()
    val doc = browser.get(url)

    val rows = doc >> element(".resultsarchive-table") >> elementList("tr")
    val rowsWithoutHeader = rows.drop(1)

    val json = mode match {
      case "races" =>
        // this block needs refactoring
        val racesResponse = RacesResponse()

        rowsWithoutHeader.foreach { row =>
          val tdList = row >> elementList("td")

          val grandPrix = tdList(1) >> text("a")
          val date = tdList(2).text
          val winner = (tdList(3) >> elementList("span")).head.text + " " + (tdList(3) >> elementList("span")) (1).text
          val car = tdList(4).text
          val laps = tdList(5).text.toInt
          val time = tdList(6).text

          val result = Some(RaceResult(grandPrix, date, winner, car, laps, time))
          racesResponse.data += result
        }

        racesResponse.asJson // for more condensed response use ".noSpaces"

      case "drivers" =>
        // this block also needs refactoring
        val driversResponse = DriversResponse()

        rowsWithoutHeader.foreach { row =>
          val tdList = row >> elementList("td")

          val pos = tdList(1).text.toInt
          val driver = (tdList(2) >> element("a") >> elementList("span")).head.text + " " + (tdList(2) >> elementList("span")) (1).text
          val nationality = tdList(3).text
          val car = tdList(4) >> text("a")
          val pts = tdList(5).text.toInt

          val standing = Some(DriverStanding(pos, driver, nationality, car, pts))
          driversResponse.data += standing
        }

        driversResponse.asJson // for more condensed response use ".noSpaces"
    }

    json.toString
  }

  def getErrorResponse(error: ErrorObject): String = {
    ErrorResponse(ListBuffer(error)).asJson.toString
  }
}

class MyScalatraServlet extends ScalatraServlet {

  get("/:mode/:year") {
    val mode: String = params("mode")
    val year: Int = params("year").toInt

    if (year < 1950 || year > 2018) {
      Response.getErrorResponse(ErrorObject("400", "Wrong year parameter", Some("Only 1950 to 2018 years are available")))

    } else mode match {
        case "races" | "drivers" => Response.getResponse(mode, year)
        case _ => Response.getErrorResponse(ErrorObject("404", "No route for '/" + mode + "'", None))
      }
  }
}
