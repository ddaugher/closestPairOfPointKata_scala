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

  def calculateDistanceBetweenPoints(x: Point, y: Point): (Pair, Double) = {
    val pair = Pair(x,y)
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

    val pairs = for(x <- points; y <- points) yield (x, y)
    Option[Pair](pairs.filter(isNotZeroDistance).map(c).sortBy(d => d._2).minBy(m => m._2)._1)
  }

  def divideAndConquer(pointsSortedByX: List[Point], pointsSortedByY: List[Point]): Pair = {
    val numPoints = pointsSortedByX.size
//    if(numPoints <= 3) {
//      return force(pointsSortedByX)
//    }

    val dividingIndex = numPoints >>> 1
    val leftOfCenter = pointsSortedByX.slice(0, dividingIndex)
    val rightOfCenter = pointsSortedByX.slice(dividingIndex, numPoints)

    var tempList = leftOfCenter.map(x => x)
    tempList = sortPointsByY(tempList)
    var closestPair = divideAndConquer(leftOfCenter, tempList)

    tempList = rightOfCenter.map(x => x)
    tempList = sortPointsByY(tempList)

    val closestPairRight = divideAndConquer(rightOfCenter, tempList)

    if (closestPairRight.distance < closestPair.distance)
      closestPair = closestPairRight

    tempList = List[Point]()
    val shortestDistance = closestPair.distance
    val centerX = rightOfCenter(0).x

    for (point <- pointsSortedByY) {
      if (Math.abs(centerX - point.x) < shortestDistance)
        tempList = tempList :+ point
    }

    closestPair = shortestDistanceF(tempList, shortestDistance, closestPair)
    closestPair
  }

  private def shortestDistanceF(tempList: List[Point], shortestDistance: Double, closestPair: Pair ): Pair = {
    var shortest = shortestDistance
    var bestResult = closestPair
    for (i <- 0 until tempList.size) {
      val point1 = tempList(i)
      for (j <- i + 1 until tempList.size) {
        val point2 = tempList(j)
        if ((point2.y - point1.y) >= shortestDistance)
          return closestPair
        val distance = point1 distance point2
        if (distance < closestPair.distance)
        {
          bestResult = Pair(point1, point2)
          shortest = distance
        }
      }
    }

    closestPair
  }

}
