import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser.JsoupElement

import scala.collection.mutable.ListBuffer

sealed trait Response

case class raceResult(grandPrix: String, date: String, winner: String, car: String, laps: Int, time: String)
case class races(results: ListBuffer[Option[raceResult]] = ListBuffer()) extends Response

object Main {
  def getURL(mode: String, year: Int): String = {
    val baseUrl = "https://www.formula1.com/en/results.html/"

    baseUrl + "/" + year + "/" + mode + ".html"
  }

  def getResponse(url: String, mode: String): String = {
    val browser = JsoupBrowser()
    val doc = browser.get(url)

    val rows = doc >> element(".resultsarchive-table") >> elementList("tr")
    val rowsWithoutHeader = rows.drop(1)

    val json = mode match {
      case "races" =>
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

  def sendToClient(response: String): Unit = {
    // replace this code with some Play response stuff
    println(response)
  }

  def main(args: Array[String]): Unit = {
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

    val url = getURL(mode, year)

    try {
      val response = getResponse(url, mode)

      sendToClient(response)

    } catch {
      case _: java.net.UnknownHostException => println("Couldn't reach the host. Please check your Internet connection.")
      case e: Exception => println("Unrecognized error occurred:", e)
    }
  }
}