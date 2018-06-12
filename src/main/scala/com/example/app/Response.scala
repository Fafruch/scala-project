package com.example.app

import scala.collection.mutable.ListBuffer
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import io.circe.generic.auto._
import io.circe.syntax._
import net.ruippeixotog.scalascraper.model.Element


sealed trait RowTrait
case class RaceResult(grandPrix: String, date: String, winner: String, car: String, laps: Int, time: String) extends RowTrait
case class DriverStanding(pos: Int, driver: String, nationality: String, car: String, pts: Int) extends RowTrait
case class ErrorObject(status: String, title: String, details: Option[String])

sealed trait ResponseTrait
case class Response(data: ListBuffer[RowTrait] = ListBuffer()) extends ResponseTrait
case class ErrorResponse(errors: ListBuffer[ErrorObject]) extends ResponseTrait

object Response {
  def getURL(mode: String, year: Int): String = {
    val baseUrl = "https://www.formula1.com/en/results.html/"

    baseUrl + "/" + year + "/" + mode + ".html"
  }

  def getRows(rows: List[Element], mode: String): ListBuffer[RowTrait] = {
    val listBuffer: ListBuffer[RowTrait] = ListBuffer()

    rows.foreach { row =>
      mode match {
        case "races" =>
          val tdList = row >> elementList("td")

          val grandPrix = tdList(1) >> text("a")
          val date = tdList(2).text
          val winner = (tdList(3) >> elementList("span")).head.text + " " + (tdList(3) >> elementList("span")) (1).text
          val car = tdList(4).text
          val laps = tdList(5).text.toInt
          val time = tdList(6).text

          listBuffer += RaceResult(grandPrix, date, winner, car, laps, time)

        case "drivers" =>
          val tdList = row >> elementList("td")

          val pos = tdList(1).text.toInt
          val driver = (tdList(2) >> element("a") >> elementList("span")).head.text + " " + (tdList(2) >> elementList("span")) (1).text
          val nationality = tdList(3).text
          val car = tdList(4) >> text("a")
          val pts = tdList(5).text.toInt

          listBuffer += DriverStanding(pos, driver, nationality, car, pts)
      }
    }

    listBuffer
  }

  def getResponse(mode: String, year: Int): String = {
    val url = getURL(mode, year)

    val browser = JsoupBrowser()
    val doc = browser.get(url)

    val rows = doc >> element(".resultsarchive-table") >> elementList("tr")
    val rowsWithoutHeader = rows.drop(1)

    val responseRows = getRows(rowsWithoutHeader, mode)

    Response(responseRows).asJson.toString
  }

  def getErrorResponse(error: ErrorObject): String = {
    ErrorResponse(ListBuffer(error)).asJson.toString
  }
}