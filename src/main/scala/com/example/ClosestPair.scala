package com.example

object ClosestPair {

  final val SIZE_OF_TUPLE: Int = 2
  final val BRUTEFORCE_THRESHHOLD: Int = 3

  case class Point(x: Double, y: Double) {
    def distance(point: Point) = math.hypot(x - point.x, y - point.y)
  }

  case class Pair(point1: Point, point2: Point) {
    val distance: Double = point1 distance point2
  }

  def bruteForce(points: List[Point]): Option[Pair] = {
    if (points.size < 2) return Option.empty[Pair]

    def isDistanceGreaterThanZero(pair: (Point, Point)): Boolean = {
      if ((pair._1 distance pair._2) > 0.0) return true
      false
    }

    type pair = (Point, Point)
    val d: pair => Pair = {
      case (point1, point2) => Pair(point1, point2)
    }

    val pairs = for {
      x <- points
      y <- points
      if isDistanceGreaterThanZero((x, y))
    } yield (x, y)

    Option[Pair](pairs.map(d).minBy(_.distance))
  }

  def divideAndConquer(points: List[Point]): Option[Pair] = {
    if (points.size <= BRUTEFORCE_THRESHHOLD) {
      return bruteForce(points)
    }

    val closestPair = closestPairBetweenHalves(points.sortBy(_.x))

    def pointFilter(point: Point): Boolean = {
      if (Math.abs(rightHalfOfList(points.sortBy(_.x)).head.x - point.x) < closestPair.distance) return true
      false
    }

    def closestWithinOverlap(points: List[Point], closestPair: Pair): Pair = {
      def _closestPair(left: Point, right: Point): Pair = {
        if ((right.y - left.y) >= closestPair.distance)
          return Pair(closestPair.point1, closestPair.point2)

        if ((left distance right) < closestPair.distance) {
          return Pair(left, right)
        }

        Pair(closestPair.point1, closestPair.point2)
      }

      type _pair = (Point, Point)
      val d: _pair => Pair = {
        case (x, y) => _closestPair(x, y)
      }

      points.sliding(SIZE_OF_TUPLE, 1).map { case List(a, b) => (a, b) }.toList.map(d).minBy(_.distance)
    }

    Option[Pair](closestWithinOverlap(points.sortBy(_.y).filter(pointFilter), closestPair))
  }

  def leftHalfOfList(points: List[Point]): List[Point] = {
    val (left, right) = points.splitAt(points.size / 2)
    left
  }

  def rightHalfOfList(points: List[Point]): List[Point] = {
    val (left, right) = points.splitAt(points.size / 2)
    right
  }

  private def closestPairBetweenHalves(points: List[Point]): Pair = {
    val closestLeftPair = divideAndConquer(leftHalfOfList(points)).get
    val closestRightPair = divideAndConquer(rightHalfOfList(points)).get

    if (closestRightPair.distance < closestLeftPair.distance)
      return closestLeftPair
    closestRightPair
  }

}
