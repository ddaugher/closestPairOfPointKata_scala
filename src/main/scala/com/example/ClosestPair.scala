package com.example

object ClosestPair {

  case class Point(x: Double, y: Double){
  }

  def sortPointsByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }


}
