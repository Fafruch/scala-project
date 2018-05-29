import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object Main {
  def urlBuilder(mode: String, year: Int): String = {
    val baseUrl = "https://www.formula1.com/en/results.html/"

    baseUrl + "/" + year + "/" + mode + ".html"
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

    val url = urlBuilder(mode, year)

    val browser = JsoupBrowser()
    val doc = browser.get(url)

    val result = doc >> elementList(".resultsarchive-table")

    println(result)
  }
}