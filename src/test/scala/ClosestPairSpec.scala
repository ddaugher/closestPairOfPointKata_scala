import org.scalatest._
import com.example.ClosestPair._

class ClosestPairSpec extends FlatSpec with Matchers {
  "Point" should "have x and y values when created" in {
  	def point = Point(1.34, 3.45)
    point.x should === (1.34)
    point.y should === (3.45)
  }
}
