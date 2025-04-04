name := "FunctionalAirportQuery"

version := "0.1"

scalaVersion := "2.13.12"

package model

case class Airport(id: String, ident: String, name: String, iso_country: String)

object Airport {
  def fromCSV(line: String): Option[Airport] = {
    val parts = line.split(",").map(_.trim.replace("\"", ""))
    if (parts.length > 8) Some(Airport(parts(0), parts(1), parts(3), parts(8))) else None
  }
}

package model

case class Runway(id: String, airport_ref: String, surface: String, le_ident: String)

object Runway {
  def fromCSV(line: String): Option[Runway] = {
    val parts = line.split(",").map(_.trim.replace("\"", ""))
    if (parts.length > 8) Some(Runway(parts(0), parts(1), parts(5), parts(8))) else None
  }
}

package model

case class Country(code: String, name: String)

object Country {
  def fromCSV(line: String): Option[Country] = {
    val parts = line.split(",").map(_.trim.replace("\"", ""))
    if (parts.length > 2) Some(Country(parts(1), parts(2))) else None
  }
}

package parser

import scala.io.Source
import model._

object CSVParser {
  def parseAirports(path: String): List[Airport] =
    Source.fromFile(path).getLines().drop(1).flatMap(Airport.fromCSV).toList

  def parseRunways(path: String): List[Runway] =
    Source.fromFile(path).getLines().drop(1).flatMap(Runway.fromCSV).toList

  def parseCountries(path: String): List[Country] =
    Source.fromFile(path).getLines().drop(1).flatMap(Country.fromCSV).toList
}

package service

import model._
import parser._

object QueryService {
  val countries = CSVParser.parseCountries("data/countries.csv")
  val airports = CSVParser.parseAirports("data/airports.csv")
  val runways = CSVParser.parseRunways("data/runways.csv")

  def runQuery(): Unit = {
    println("Enter a country code or name:")
    val input = scala.io.StdIn.readLine().toLowerCase

    val foundCountries = countries.filter(c => c.code.toLowerCase == input || c.name.toLowerCase.contains(input))

    foundCountries.foreach { country =>
      println(s"\nAirports in ${country.name} (${country.code}):")
      val countryAirports = airports.filter(_.iso_country == country.code)
      countryAirports.foreach { airport =>
        println(s"  - ${airport.name}")
        val airportRunways = runways.filter(_.airport_ref == airport.id)
        airportRunways.foreach(r => println(s"      Runway: surface=${r.surface}, ident=${r.le_ident}"))
      }
    }

    if (foundCountries.isEmpty) println("No matching country found.")
  }

  def showReports(): Unit = {
    println("\nReports Menu:")
    println("1. Top 10 countries with most/least airports")
    println("2. Runway types by country")
    println("3. Top 10 most common runway latitudes")
    val choice = scala.io.StdIn.readLine()

    choice match {
      case "1" =>
        val grouped = airports.groupBy(_.iso_country).mapValues(_.size)
        val top10 = grouped.toList.sortBy(-_._2).take(10)
        val bottom10 = grouped.toList.sortBy(_._2).take(10)

        println("\nTop 10 Countries with Most Airports:")
        top10.foreach { case (code, count) =>
          val name = countries.find(_.code == code).map(_.name).getOrElse(code)
          println(s"$name: $count airports")
        }

        println("\nBottom 10 Countries with Least Airports:")
        bottom10.foreach { case (code, count) =>
          val name = countries.find(_.code == code).map(_.name).getOrElse(code)
          println(s"$name: $count airports")
        }

      case "2" =>
        countries.foreach { country =>
          val aps = airports.filter(_.iso_country == country.code)
          val rws = runways.filter(r => aps.exists(_.id == r.airport_ref)).map(_.surface).distinct
          println(s"${country.name}: ${rws.mkString(", ")}")
        }

      case "3" =>
        val topLat = runways.map(_.le_ident).filter(_.nonEmpty)
          .groupBy(identity).mapValues(_.size)
          .toList.sortBy(-_._2).take(10)

        println("\nTop 10 Most Common Runway Identifiers:")
        topLat.foreach { case (ident, count) => println(s"$ident: $count times") }

      case _ => println("Invalid choice.")
    }
  }
}

package ui

import service._

object MainApp extends App {
  println("Welcome to the Airport Query App!")
  println("Type 'query' or 'reports':")

  scala.io.StdIn.readLine() match {
    case "query"   => QueryService.runQuery()
    case "reports" => QueryService.showReports()
    case _         => println("Invalid option")
  }
}