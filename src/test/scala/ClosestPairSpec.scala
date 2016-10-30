import org.scalatest._
import com.example.ClosestPair
import com.example.ClosestPair.Point
import com.example.ClosestPair.Pair

class ClosestPairSpec extends FlatSpec with Matchers {
  "Point" should "have x and y values when created" in {
  	def point = Point(1.34, 3.45)
    point.x should === (1.34)
    point.y should === (3.45)
  }

  "ClosestPair" should "sort list of Points by x" in {
  	def point1 = Point(0.00, 0.00)
  	def point2 = Point(1.00, 1.00)
  	def point3 = Point(2.00, 2.00)
  	val points = List[Point](point3, point2, point1)
    ClosestPair.sortPointsByX(points) should === (List[Point](point1, point2, point3))
  }

  "ClosestPair" should "sort list of Points by y" in {
  	def point1 = Point(0.00, 0.00)
  	def point2 = Point(1.00, 1.00)
  	def point3 = Point(2.00, 2.00)
  	val points = List[Point](point3, point2, point1)
  	ClosestPair.sortPointsByY(points) should === (List[Point](point1, point2, point3))
  }

  "leftHalfOfList" should "return first point when list of three provided" in {
    def point1 = Point(0.00, 0.00)
    def point2 = Point(1.00, 1.00)
    def point3 = Point(2.00, 2.00)
    val points = List[Point](point3, point2, point1)
    val leftHalf = ClosestPair.leftHalfOfList(points)
    leftHalf.size should === (1)
    true should === (leftHalf.contains(point3))
  }

  "leftHalfOfList" should "return first two points when list of four provided" in {
    def point1 = Point(0.00, 0.00)
    def point2 = Point(1.00, 1.00)
    def point3 = Point(2.00, 2.00)
    def point4 = Point(3.00, 3.00)
    val points = List[Point](point4, point3, point2, point1)
    val leftHalf = ClosestPair.leftHalfOfList(points)
    leftHalf.size should === (2)
    true should === (leftHalf.contains(point4))
    true should === (leftHalf.contains(point3))
  }

  "rightHalfOfList" should "return last two points when list of three provided" in {
    def point1 = Point(0.00, 0.00)
    def point2 = Point(1.00, 1.00)
    def point3 = Point(2.00, 2.00)
    val points = List[Point](point3, point2, point1)
    val rightHalf = ClosestPair.rightHalfOfList(points)
    rightHalf.size should === (2)
    true should === (rightHalf.contains(point1))
    true should === (rightHalf.contains(point2))
  }

  "rightHalfOfList" should "return last two points when list of four provided" in {
    def point1 = Point(0.00, 0.00)
    def point2 = Point(1.00, 1.00)
    def point3 = Point(2.00, 2.00)
    def point4 = Point(3.00, 3.00)
    val points = List[Point](point4, point3, point2, point1)
    val rightHalf = ClosestPair.rightHalfOfList(points)
    rightHalf.size should === (2)
    true should === (rightHalf.contains(point1))
    true should === (rightHalf.contains(point2))
  }

  "Point" should "return distance between two points" in {
  	def point = Point(2.0, 2.0)
  	assert (point.distance(Point(3.0, 3.0)) === 1.4142135623730951)
  }

  "Pair" should "have two points when created" in {
  	def point1 = Point(0.00, 0.00)
  	def point2 = Point(1.00, 1.00)
  	def pair = Pair(point1, point2)
  	pair.point1 should === (point1)
  	pair.point2 should === (point2)
  }

  "Pair" should "return distance between its two points" in {
  	def point1 = Point(0.00, 0.00)
  	def point2 = Point(1.00, 1.00)
  	def pair = Pair(point1, point2)
  	pair.distance should === (1.4142135623730951)
  }

  "bruteForce" should "return None when no points supplied" in {
    ClosestPair.bruteForce(List[Point]()) should === (None)
  }

  "bruteForce" should "return None when one Point supplied" in {
    ClosestPair.bruteForce(List[Point](Point(0.00, 1.00))) should === (None)
  }

  "bruteForce" should "return Pair with same two Points when two Points supplied" in {
    ClosestPair.bruteForce(List[Point](Point(0.0, 0.0), Point(1.00, 1.00))).get should === (Pair(Point(0.0,0.0),Point(1.0,1.0)))
  }

  "bruteForce" should "return proper proper Pair when three Points supplied" in {
    def point1 = Point(0.00, 0.00)
    def point2 = Point(1.00, 1.00)
    def point3 = Point(2.00, 3.00)
    ClosestPair.bruteForce(List[Point](point1, point2, point3)).get should === (Pair(Point(0.0,0.0),Point(1.0,1.0)))
  }

  "bruteForce" should "return proper Pair when five Points supplied" in {
    def point1 = Point(10.00, 5.00)
    def point2 = Point(11.00, 10.00)
    def point3 = Point(21.00, 30.00)
    def point4 = Point(20.00, 3.00)
    def point5 = Point(2.00, 30.00)
    ClosestPair.bruteForce(List[Point](point1, point2, point3, point4, point5)).get should === (Pair(Point(10.0,5.0),Point(11.0,10.0)))
  }

  "divideAndConquer" should "return proper shortest distance" in {
    def point1 = Point(1.00, 1.00)
    def point2 = Point(2.00, 2.00)
    def point3 = Point(10.00, 10.00)
    def point4 = Point(-10.00, -10.00)
    def point5 = Point(-20.00, -20.00)
    ClosestPair.divideAndConquer(List[Point](point1, point2, point3, point4, point5)).get should === (Pair(point1, point2))
  }

  "divideAndConquer" should "return closest points, utilizing 'bruteForce' method when number of points equal 3" in {
    def point1 = Point(10.00, 5.00)
    def point2 = Point(11.00, 10.00)
    def point3 = Point(21.00, 30.00)
    ClosestPair.divideAndConquer(List[Point](point1, point2, point3)).get should === (Pair(point1, point2))
  }
}
