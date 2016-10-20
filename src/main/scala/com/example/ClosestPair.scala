package com.example

object ClosestPair {

  case class Point(x: Double, y: Double){
    def distance(p: Point) = math.hypot(x-p.x, y-p.y)
  }

  def sortPointsByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }

  def sortPointsByY(points: List[Point]) = {
    points.sortBy(point => point.y)
  }


}
