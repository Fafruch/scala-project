import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

object Main {
  def main(args: Array[String]): Unit = {
    val browser = JsoupBrowser()
    val doc = browser.get("https://www.formula1.com/en/results.html/2018/races/984/monaco/fastest-laps.html")

    // Extract the text inside the element with id "header"
    val result = doc >> elementList(".resultsarchive-table")
    // res2: String = Test page h1

    println(result)
  }
}