import com.example.ClosestPair
import com.example.ClosestPair.Point
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
  	assert (ClosestPair.sortPointsByX(points) === List[Point](point1, point2, point3))
  }

  "ClosestPair" should "sort list of Points by y" in {
  	def point1 = Point(0.00, 0.00)
  	def point2 = Point(1.00, 1.00)
  	def point3 = Point(2.00, 2.00)
  	val points = List[Point](point3, point2, point1)
  	assert (ClosestPair.sortPointsByY(points) === List[Point](point1, point2, point3))
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
  	assert (pair.distance === 1.4142135623730951)
  }

  "force" should "return None when no points supplied" in {
    assert (ClosestPair.force(List[Point]()) === None)
  }

  "force" should "return None when one Point supplied" in {
    assert (ClosestPair.force(List[Point](Point(0.00, 1.00))) === None)  
  }

  "force" should "return Pair with same two Points when two Points supplied" in {
    assert (ClosestPair.force(List[Point](Point(0.0, 0.0), Point(1.00, 1.00))).get === Pair(Point(0.0,0.0),Point(1.0,1.0)))
  }

  "force" should "return proper proper Pair when three Points supplied" in {
    def point1 = Point(0.00, 0.00)
    def point2 = Point(1.00, 1.00)
    def point3 = Point(2.00, 3.00)
    assert (ClosestPair.force(List[Point](point1, point2, point3)).get === Pair(Point(0.0,0.0),Point(1.0,1.0)))
  }

  "force" should "return proper Pair when five Points supplied" in {
    def point1 = Point(10.00, 5.00)
    def point2 = Point(11.00, 10.00)
    def point3 = Point(21.00, 30.00)
    def point4 = Point(20.00, 3.00)
    def point5 = Point(2.00, 30.00)
    assert (ClosestPair.force(List[Point](point1, point2, point3, point4, point5)).get === Pair(Point(10.0,5.0),Point(11.0,10.0)))
  }

//  "divideAndConquer" should "return proper shortest distance" in {
//    def point1 = Point(10.00, 5.00)
//    def point2 = Point(11.00, 10.00)
//    def point3 = Point(21.00, 30.00)
//    def point4 = Point(20.00, 3.00)
//    def point5 = Point(2.00, 30.00)
//    assert (ClosestPair.divideAndConquer(List[Point](point1, point2, point3, point4, point5), List[Point](point1, point2, point3, point4, point5)) === Pair(point1, point2))
//  }
}
