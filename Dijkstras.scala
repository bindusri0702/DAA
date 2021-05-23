
import Dijkstras._
object Dijkstras {
  

  // from the set of vertices not yet included in shortest path tree
  val V: Int = 9

  // Driver method
  def main(args: Array[String]): Unit = {
    /* Let us create the example graph discussed above */

    val graph: Array[Array[Int]] = Array(
      Array(0, 4, 0, 0, 0, 0, 0, 8, 0),
      Array(4, 0, 8, 0, 0, 0, 0, 11, 0),
      Array(0, 8, 0, 7, 0, 4, 0, 0, 2),
      Array(0, 0, 7, 0, 9, 14, 0, 0, 0),
      Array(0, 0, 0, 9, 0, 10, 0, 0, 0),
      Array(0, 0, 4, 14, 10, 0, 2, 0, 0),
      Array(0, 0, 0, 0, 0, 2, 0, 1, 6),
      Array(8, 11, 0, 0, 0, 0, 1, 0, 7),
      Array(0, 0, 2, 0, 0, 0, 6, 7, 0)
    )
    val t: ShortestPath = new ShortestPath()
    val t_start = System.nanoTime()
    t.dijkstra(graph, 0)
    val t_end = System.nanoTime()
    println("time taken by Dijkstras in seconds " + (t_end - t_start)/1.0e9)
    val runTime = Runtime.getRuntime()
    val space = runTime.totalMemory() - runTime.freeMemory()
    println("space utilised by Dijkstras algorithm in bytes " + space)
  }

}

class ShortestPath {

  def minDistance(dist: Array[Int], sptSet: Array[java.lang.Boolean]): Int = {
    // Initialize min value
    var min: Int = java.lang.Integer.MAX_VALUE
    var min_index: Int = -1
    for (v <- 0 until V if sptSet(v) == false && dist(v) <= min) {
      min = dist(v)
      min_index = v
    }
    min_index
  }

  // A utility function to print the constructed distance array
  def printSolution(dist: Array[Int]): Unit = {
    println("Vertex \t\t Distance from Source")
    for (i <- 0 until V) println(i + " \t\t " + dist(i))
  }

  // representation
  def dijkstra(graph: Array[Array[Int]], src: Int): Unit = {
    // The output array. dist[i] will hold
    val dist: Array[Int] = Array.ofDim[Int](V)
    // path tree or shortest distance from src to i is finalized
    val sptSet: Array[java.lang.Boolean] = Array.ofDim[java.lang.Boolean](V)
    for (i <- 0 until V) {
      dist(i) = java.lang.Integer.MAX_VALUE
      sptSet(i) = false
    }
    // Distance of source vertex from itself is always 0
    dist(src) = 0
    for (count <- 0 until V - 1) {
      // iteration.
      val u: Int = minDistance(dist, sptSet)
      // Mark the picked vertex as processed
      sptSet(u) = true
      for (v <- 0 until V
           if !sptSet(v) && graph(u)(v) != 0 && dist(u) != java.lang.Integer.MAX_VALUE &&
             dist(u) + graph(u)(v) < dist(v)) dist(v) = dist(u) + graph(u)(v)
    }
    // Pick the minimum distance vertex from the set of vertices
    // not yet processed. u is always equal to src in first
    // Update dist value of the adjacent vertices of the
    // Pick the minimum distance vertex from the set of vertices
    // not yet processed. u is always equal to src in first
    // Update dist value of the adjacent vertices of the
    // print the constructed distance array
    printSolution(dist)
  }
  // the shortest distance from src to i
  // sptSet[i] will true if vertex i is included in shortest
  // the shortest distance from src to i
  // sptSet[i] will true if vertex i is included in shortest


 
}
