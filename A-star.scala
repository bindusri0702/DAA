

object Daa_Astar {
  
  
  def main(args: Array[String]) { 
    val t_start = System.nanoTime()
    Astar('A', 'G')
    val t_end   = System.nanoTime()
    println("Time taken by A-star in seconds " + (t_end - t_start)/1.0e9)
    val runTime = Runtime.getRuntime()
    val space = runTime.totalMemory() - runTime.freeMemory()
    println("space utilised by A star algorithm in bytes " + space)
  }  
  
  def Astar(start_node: Char, stop_node: Char): List[Char] = {
    var route: List[Char] = List()
    var open_set = Set(start_node)
    var closed_set: Set[Char] = Set()
    var g = scala.collection.mutable.Map[Char, Int]()
    var parents: Map[Char, Char] = Map(start_node -> start_node)
    
    g += (start_node -> 0)
    while (open_set.size > 0) {
      var n: Char = '@'
      for (v <- open_set) {
        if (n == '@' || g(v) + heuristic(v) < g(n) + heuristic(n)) n = v }
      if (n == stop_node || graph_nodes(n) == None) println("Completed Processing Nodes")
      else {
        for ((m, wt) <- get_neighbors(n)) {
          if (open_set.contains(m) == false & closed_set.contains(m) == false) {
            open_set += m
            parents += (m -> n)
            g(m) = g(n) + wt.toString.toInt } 
          else {
            if (g(m) > g(n) + wt.toString.toInt) {
              g(m) = g(n) + wt.toString.toInt
              parents += (m -> n)

              if (closed_set.contains(m) == true) {
                closed_set -= m
                open_set += m } } }
              }
             }
      if (n == '@') {
        print("route doesn't exist")
        return (Nil) }
      if (n == stop_node) {
        while (parents(n) != n) {
          route = n :: route
          n = parents(n) }
        route = start_node :: route
        //route.reverse
        println("route found")
        println(route)
        return (route)  }
      open_set -= n
      closed_set += n  }
      println("route doesn't exist")
      return (Nil)  }

  def heuristic(n: Char): Int = {
    var H_dist: Map[Char, Int] = Map('A' -> 21, 'B' -> 14, 'C' -> 18, 'D' -> 18, 'E' -> 5, 'F' -> 8, 'G' -> 0)
    return (H_dist(n))  }

  def get_neighbors(v: Char): Map[Char, Any] = {
    if (graph_nodes.contains(v)) return (graph_nodes(v))
    else return Map()  }

  val graph_nodes = Map(
    'A' -> Map('B' -> 9, 'C' -> 4, 'D' -> 7),
    'B' -> Map('A' -> 9, 'E' -> 11),
    'C' -> Map('A' -> 4, 'E' -> 17, 'F' -> 12),
    'D' -> Map('A' -> 7, 'F' -> 14),
    'E' -> Map('B' -> 11, 'C' -> 17, 'G' -> 5),
    'F' -> Map('C' -> 12, 'D' -> 14, 'G' -> 9),
    'G' -> Map('E' -> 5, 'F' -> 9))

}
