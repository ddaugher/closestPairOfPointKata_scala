package com.example

object ClosestPair {

  final val SIZE_OF_TUPLE:Int = 2
  final val BRUTEFORCE_THRESHHOLD:Int = 3

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

    type pair = (Point, Point)
    val d: pair => (Pair, Double) = (t) => distanceBetweenTwoPoints(t._1, t._2)

    val pairs = for (x <- points; y <- points) yield (x, y)
    Option[Pair](pairs.filter(isDistanceGreaterThanZero).map(d).sortBy(distance => distance._2).minBy(distance => distance._2)._1)
  }

  def divideAndConquer(points: List[Point]): Option[Pair] = {
    if (points.size <= BRUTEFORCE_THRESHHOLD) {
      return bruteForce(points)
    }

    val closestPair = closestPairBetweenHalves(sortPointsByX(points))

    def pointFilter(point: Point): Boolean = {
      if (Math.abs(rightHalfOfList(sortPointsByX(points))(0).x - point.x) < closestPair.distance) return true
      false
    }

    def shortestVerticalOverlap(points: List[Point], shortestDistance: Double, closestPair: Pair): Pair = {

      def distance(left: Point, right: Point): Pair = {
        if ((right.y - left.y) >= shortestDistance)
          return Pair(closestPair.point1, closestPair.point2)

        if ((left distance right) < closestPair.distance) {
          return Pair(left, right)
        }

        Pair(closestPair.point1, closestPair.point2)
      }

      type pair = (Point, Point)
      val d: pair => Pair = (t) => distance(t._1, t._2)

      points.sliding(SIZE_OF_TUPLE, 1).map { case List(a, b) => (a, b) }.toList.map(d).sortBy(_.distance).minBy(_.distance)
    }

    Option[Pair](shortestVerticalOverlap(sortPointsByY(points).filter(pointFilter), closestPair.distance, closestPair))
  }

  def leftHalfOfList(points: List[Point]): List[Point] = {
    points.slice(0, points.size >>> 1)
  }

  def rightHalfOfList(points: List[Point]): List[Point] = {
    points.slice(points.size >>> 1, points.size)
  }

  private def closestPairBetweenHalves(points: List[Point]): Pair = {
    val closestLeftPair = divideAndConquer(leftHalfOfList(points)).get
    val closestRightPair = divideAndConquer(rightHalfOfList(points)).get

    if (closestRightPair.distance < closestLeftPair.distance)
      return closestLeftPair
    closestRightPair
  }

}
