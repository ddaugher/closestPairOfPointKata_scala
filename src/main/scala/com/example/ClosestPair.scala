package com.example

object ClosestPair {

  case class Point(x: Double, y: Double){
    def distance(p: Point) = math.hypot(x-p.x, y-p.y)
  }

  case class Pair(point1: Point, point2: Point){
    val distance: Double = point1 distance point2
  }

  def sortPointsByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }

  def sortPointsByY(points: List[Point]) = {
    points.sortBy(point => point.y)
  }

  def force(points: List[Point]): Option[Double] = {
    val none: Option[Double] = None
    if (points.size < 2) return none
    val d = Pair(points.head, points.last).distance
    Option[Double](d)
  }

}
