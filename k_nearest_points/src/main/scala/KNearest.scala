
import java.util.Comparator
import scala.collection.*

object KNearest {

  case class Point( x:Double,
                    y:Double
                  ) {
    import math.{pow, sqrt}
    def distanceTo(other:Point): Double =
      sqrt(pow(other.x - x, 2) + pow(other.y - y, 2))
  }

  case class User( id: String,
                   location: Point
                 )

  // Constant space as input is modified in place
  // O(n log n) time due to in-place array sort.
  def findNearestK_v1(u:User, k:Int, knownPoints:Array[Point]): Seq[Point] = {
    knownPoints.sortBy(p => p.distanceTo(u.location))
    knownPoints.slice(0, k)
  }


  // O(k) space for heap
  // O(n) time for traversing knownPoints
  // O(log n) time for insert
  // O(log n) time for pop
  //  total O(n * (2 log n))

  def findNearestK_v2(u: User, k: Int, knownPoints: IterableOnce[Point]): (Long, Seq[Point]) = {

    var comparisonCount = 0L

    implicit val furthestOrdering: Ordering[Point] =
      (x: Point, y: Point) => {
          comparisonCount += 1
          x.distanceTo(u.location).compare(y.distanceTo(u.location))
        }

    val furthestHeap = collection.mutable.PriorityQueue.empty[Point]
    knownPoints.iterator.foreach { p =>
      furthestHeap.addOne(p)
      if(furthestHeap.size > k) {
        furthestHeap.dequeue()
      }
    }
    (comparisonCount, furthestHeap.toSeq)
  }


  def main(args: Array[String]) = {
    val randomPoints = (0 to 10000).iterator.map { _ =>
      Point(util.Random.between(-10.0,10.0), util.Random.between(-10.0,10.0))
    }

    val user = User("user-1", Point(0, 0))

    val (compareCount, points) = findNearestK_v2(user, 10, randomPoints)

    println(s"$compareCount comparisons, ${points.size} points returned: ${points.mkString("\n")}")
  }

  //    100 points =       220 comparisons, 100 points returned
  //   1000 points =     12030 comparisons, 100 points returned
  //  10000 pointa =    101289 comparisons, 100 points returned
  // 100000 points =    879606 comparisons, 100 points returned
  //     1m points =   8043312 comparisons, 100 points returned
  //    10m points =  79943932 comparisons, 100 points returned
  //   100m points = 798823502 comparisons, 100 points returned
}
