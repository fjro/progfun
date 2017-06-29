package observatory

import java.time.LocalDate

case class Location(lat: Double, lon: Double)

case class StationID(stnID: Int, wbanID: Int) //{
//  override def equals(o: scala.Any): Boolean = {
//    o match {
//      case StationID => if (stnID)
//    }
//  }
//}
case class Station(stationID: StationID, location: Location)
case class Observation(stationID: StationID, date: LocalDate, temp: Double)

case class Color(red: Int, green: Int, blue: Int)

