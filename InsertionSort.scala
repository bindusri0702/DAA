import java.io._
import util.Random

object InsertionSort  {
  //val ass:Array[Int] = Array(0, 5, 2, 6, 3, 1)

  def main(args: Array[String]) {

    val p = scala.util.Random
    val ls = Random.shuffle((for (i <- 1 to 10) yield p.nextInt(100)).toList)

    val arr=ls.toArray
    println("original list " + arr.toList)
    insertionSortAscending(arr)
    println("sorted list "+ arr.toList)

    val end = 200000                                //> end  : Int = 200000
    val begn = 1                                    //> begn  : Int = 1
    val inc = 10000                                 //> inc  : Int = 10000
    val mess = (begn to end by inc).toArray         //> mess  : Array[Int] = Array(1, 10001, 20001, 30001, 40001, 50001, 60001, 700

    var fibTime: Array[Long] = Array()              //> fibTime  : Array[Long] = Array()
    var Mem: Array[Long] = Array()                  //> Mem  : Array[Long] = Array()

    for (i <- mess) {
      var avgTime: Array[Long] = Array()
      var avgMem: Array[Long] = Array()
      for (_ <- 0 to 5) {
        val x = Array.fill(i)(10000).map(Random.nextInt)
        val t0 = System.nanoTime()
        insertionSortAscending(x)
        val t1 = System.nanoTime() - t0
        avgTime = avgTime :+ t1
        val runtime = Runtime.getRuntime()
        runtime.gc()
        val s2 = runtime.totalMemory() - runtime.freeMemory()
        avgMem = avgMem :+ s2
      }
      fibTime = fibTime :+ (avgTime.reduce(_ + _) / avgTime.length)
      Mem = Mem :+ (avgMem.reduce(_ + _) / avgMem.length)
    }
    val time = fibTime
    val memory = Mem

    val file = new File("C:/Users/bindu/Desktop/insertion.csv")

    val bw = new BufferedWriter(new FileWriter(file))

    for (n <- 1 until time.length) {
      bw.write(mess(n).toString + "," + time(n).toString + "," + memory(n).toString + "\n")
    }
    bw.close()

  }

  def insertionSortAscending(array:Array[Int]) {
    //first element is considered as sorted sublist
    for (outer <- 1 until array.length) {

      var temp = array.apply(outer)

      var inner = outer - 1
      // temp < array.apply(inner) is used for Ascending sort
      while (inner >= 0 && temp < array.apply(inner)) {

        array.update(inner + 1, array.apply(inner))

        inner -= 1

      }

      array.update(inner + 1, temp)

    }

  }
 
}
