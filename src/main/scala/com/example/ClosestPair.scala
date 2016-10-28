package com.example

object ClosestPair {

  case class Point(x: Double, y: Double) {
    def distance(point: Point) = math.hypot(x - point.x, y - point.y)
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

  def bruteForce(points: List[Point]): Option[Pair] = {
    val none: Option[Pair] = None
    if (points.size < 2) return none

    def isDistanceGreaterThanZero(pair: (Point, Point)): Boolean = {
      if (pair._1.distance(pair._2) > 0.0) return true
      false
    }

    def distanceBetweenTwoPoints(point1: Point, point2: Point): (Pair, Double) = {
      val pair = Pair(point1, point2)
      (pair, pair.distance)
    }

    type p = (Point, Point)
    val c: p => (Pair, Double) = (t) => distanceBetweenTwoPoints(t._1, t._2)

    val pairs = for (x <- points; y <- points) yield (x, y)
    Option[Pair](pairs.filter(isDistanceGreaterThanZero).map(c).sortBy(distance => distance._2).minBy(distance => distance._2)._1)
  }

  def divideAndConquer(points: List[Point]): Option[Pair] = {
    if (points.size <= 3) {
      return bruteForce(points)
    }

    val closestPair = closestPairBetweenHalves(sortPointsByX(points))

    def pointFilter(point: Point): Boolean = {
      if (Math.abs(rightHalfOfList(sortPointsByX(points))(0).x - point.x) < closestPair.distance) return true
      false
    }

    val tempList = sortPointsByY(points).filter(pointFilter)

    def shortestVerticalOverlap(points: List[Point], shortestDistance: Double, closestPair: Pair): Pair = {

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

      points.sliding(2, 1).map { case List(a, b) => (a, b) }.toList.map(d).sortBy(_.distance).minBy(_.distance)
    }

    Option[Pair](shortestVerticalOverlap(tempList, closestPair.distance, closestPair))
  }



  private def leftHalfOfList(points: List[Point]): List[Point] = {
    points.slice(0, points.size >>> 1)
  }

  private def rightHalfOfList(points: List[Point]): List[Point] = {
    points.slice(points.size >>> 1, points.size)
  }

  private def closestPairBetweenHalves(points: List[Point]): Pair = {
    val closestPair = divideAndConquer(leftHalfOfList(points)).get
    val closestPairRight = divideAndConquer(rightHalfOfList(points)).get

    if (closestPairRight.distance < closestPair.distance)
      return closestPair
    closestPairRight
  }

}
