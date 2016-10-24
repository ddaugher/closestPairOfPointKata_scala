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

  def calculateDistanceBetweenPoints(left: Point, right: Point): Double = {
    Pair(left, right).distance
  }

  def isNotZeroDistance(pair: (Point, Point)): Boolean = {
    if (pair._1.distance(pair._2) != 0.0) return true
    false
  }

  def force(points: List[Point]): Option[Double] = {
    val none: Option[Double] = None
    if (points.size < 2) return none

    type Pair = (Point, Point)
    val c: Pair => Double = (t) =>  calculateDistanceBetweenPoints(t._1, t._2)

    val pairs = for(x <- points; y <- points) yield (x, y)
    Option[Double](pairs.filter(isNotZeroDistance).map(c).min)
  }

}
