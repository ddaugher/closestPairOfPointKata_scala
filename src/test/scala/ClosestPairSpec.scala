import scala.collection.mutable.ListBuffer
import org.scalatest._
import com.example.ClosestPair
import com.example.ClosestPair.Point

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
}
