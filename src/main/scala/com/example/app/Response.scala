package com.example.app

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import io.circe.generic.auto._
import io.circe.syntax._
import net.ruippeixotog.scalascraper.model.Element


sealed trait RowTrait
case class race(grandPrix: String, date: String, winner: String, car: String, laps: Int, time: String) extends RowTrait
case class standing(pos: Int, driver: String, nationality: String, car: String, pts: Int) extends RowTrait
case class teamStanding(pos: Int, team: String, pts: Int) extends RowTrait
case class fastestLap(grandPrix: String, driver: String, car: String, time: String) extends RowTrait
case class ErrorObject(status: String, title: String, details: Option[String])

sealed trait ResponseTrait
case class Response(data: List[RowTrait] = List()) extends ResponseTrait
case class ErrorResponse(errors: List[ErrorObject]) extends ResponseTrait

object Response {
  def getURL(mode: String, year: Int): String = {
    val baseUrl = "https://www.formula1.com/en/results.html/"

    baseUrl + "/" + year + "/" + mode + ".html"
  }

  def getRows(rows: List[Element], mode: String): List[RowTrait] = {
    rows.map { row =>
      mode match {
        case "races" =>
          val tdList = row >> elementList("td")

          val grandPrix = tdList(1) >> text("a")
          val date = tdList(2).text
          val winner = (tdList(3) >> elementList("span")).head.text + " " + (tdList(3) >> elementList("span"))(1).text
          val car = tdList(4).text
          val laps = tdList(5).text.toInt
          val time = tdList(6).text

          race(grandPrix, date, winner, car, laps, time)

        case "drivers" =>
          val tdList = row >> elementList("td")

          val pos = tdList(1).text.toInt
          val driver = (tdList(2) >> element("a") >> elementList("span")).head.text + " " + (tdList(2) >> elementList("span"))(1).text
          val nationality = tdList(3).text
          val car = tdList(4) >> text("a")
          val pts = tdList(5).text.toInt

          standing(pos, driver, nationality, car, pts)

        case "team" =>
          val tdList = row >> elementList("td")

          val pos = tdList(1).text.toInt
          val team = tdList(2) >> text("a")
          val pts = tdList(3).text.toInt

          teamStanding(pos, team, pts)

        case "fastest-laps" =>
          val tdList = row >> elementList("td")

          val grandPrix = tdList(1).text
          val driver = (tdList(2) >> elementList("span")).head.text + " " + (tdList(2) >> elementList("span"))(1).text
          val car = tdList(3).text
          val time = tdList(4).text

          fastestLap(grandPrix, driver, car, time)
      }
    }
  }

  def getResponse(mode: String, year: Int): String = {
    val url = getURL(mode, year)

    val doc = JsoupBrowser().get(url)

    val rows = doc >> element(".resultsarchive-table") >> elementList("tr")
    val rowsWithoutHeader = rows.drop(1)

    val responseRows = getRows(rowsWithoutHeader, mode)

    Response(responseRows).asJson.toString
  }

  def getErrorResponse(error: ErrorObject): String = {
    ErrorResponse(List(error)).asJson.toString
  }
}