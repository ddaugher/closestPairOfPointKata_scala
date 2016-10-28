package com.example

object ClosestPair {

  case class Point(x: Double, y: Double) {
    def distance(p: Point) = math.hypot(x - p.x, y - p.y)
  }

  case class Pair(point1: Point, point2: Point) {
    val distance: Double = point1 distance point2
  }

  def sortPointsByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }

  def sortPointsByY(points: List[Point]) = {
    points.sortBy(point => point.y)
  }

  def calculateDistanceBetweenPoints(x: Point, y: Point): (Pair, Double) = {
    val pair = Pair(x, y)
    (pair, pair.distance)
  }

  def isNotZeroDistance(pair: (Point, Point)): Boolean = {
    if (pair._1.distance(pair._2) != 0.0) return true
    false
  }

  def force(points: List[Point]): Option[Pair] = {
    val none: Option[Pair] = None
    if (points.size < 2) return none

    type p = (Point, Point)
    val c: p => (Pair, Double) = (t) => calculateDistanceBetweenPoints(t._1, t._2)

    val pairs = for (x <- points; y <- points) yield (x, y)
    Option[Pair](pairs.filter(isNotZeroDistance).map(c).sortBy(distance => distance._2).minBy(distance => distance._2)._1)
  }

  def divideAndConquer(list: List[Point]): Option[Pair] = {
    if (list.size <= 3) {
      return force(list)
    }

    val closestPair = closestPairBetweenHalves(sortPointsByX(list))

    def pointFilter(point: Point): Boolean = {
      if (Math.abs(rightHalfOfList(sortPointsByX(list))(0).x - point.x) < closestPair.distance) return true
      false
    }

    val tempList = sortPointsByY(list).filter(pointFilter)

    def shortestVerticalOverlap(list: List[Point], shortestDistance: Double, closestPair: Pair): Pair = {

      def distance(left: Point, right: Point): Pair = {
        if ((right.y - left.y) >= shortestDistance)
          return Pair(closestPair.point1, closestPair.point2)

        if ((left distance right) < closestPair.distance) {
          return Pair(left, right)
        }

        Pair(closestPair.point1, closestPair.point2)
      }

      type p = (Point, Point)
      val d: p => Pair = (t) => distance(t._1, t._2)

      list.sliding(2, 1).map { case List(a, b) => (a, b) }.toList.map(d).sortBy(_.distance).minBy(_.distance)
    }

    Option[Pair](shortestVerticalOverlap(tempList, closestPair.distance, closestPair))
  }



  private def leftHalfOfList(list: List[Point]): List[Point] = {
    list.slice(0, list.size >>> 1)
  }

  private def rightHalfOfList(list: List[Point]): List[Point] = {
    list.slice(list.size >>> 1, list.size)
  }

  private def closestPairBetweenHalves(list: List[Point]): Pair = {
    val closestPair = divideAndConquer(leftHalfOfList(list)).get
    val closestPairRight = divideAndConquer(rightHalfOfList(list)).get

    if (closestPairRight.distance < closestPair.distance)
      return closestPair
    closestPairRight
  }

}
