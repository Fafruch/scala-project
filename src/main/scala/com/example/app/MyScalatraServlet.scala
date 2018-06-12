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

case class raceResult(grandPrix: String, date: String, winner: String, car: String, laps: Int, time: String)
case class races(results: ListBuffer[Option[raceResult]] = ListBuffer()) extends ResponseTrait


object Response {
  def getArgs(args: Array[String]): (String, Int) = {
    if (args.length < 2) {
      throw new Exception("Not enough arguments. Please pass mode and year.")
    }

    val mode = args(0)
    if (mode != "races" && mode != "drivers") {
      throw new Exception("Wrong mode.")
    }

    val year = args(1).toInt
    if (year < 1950 && year > 2018) {
      throw new Exception("Wrong year.")
    }

    (mode, year)
  }

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
        val racesResponse = races()

        rowsWithoutHeader.foreach { row =>
          val tdList = row >> elementList("td")

          val grandPrix = tdList(1) >> text("a")
          val date = tdList(2).text
          val winner = (tdList(3) >> elementList("span")).head.text + " " + (tdList(3) >> elementList("span"))(1).text
          val car = tdList(4).text
          val laps = tdList(5).text.toInt
          val time = tdList(6).text

          val result = Some(raceResult(grandPrix, date, winner, car, laps, time))
          racesResponse.results += result
        }

        racesResponse.asJson // for more condensed response use ".noSpaces"

      case "drivers" =>
      // do something similar
    }

    json.toString
  }
}

class MyScalatraServlet extends ScalatraServlet {

  get("/:mode/:year") {
    val mode: String = params("mode")
    val year: Int = params("year").toInt

    Response.getResponse(mode, year)
  }
}
