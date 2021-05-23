import java.io._
import util.Random

object MergeSort {
  def sort(array:Array[Int]) {
    if (array.length > 1 ){
      var firstArrayLength = (array.length/2)
      var first:Array[Int] = array.slice(0, firstArrayLength)
      var second:Array[Int] = array.slice(firstArrayLength, array.length)
      sort(first)
      sort(second)
      merge(array, first, second)
    }
  }

  def merge(result:Array[Int], first:Array[Int], second:Array[Int]) {
    var i:Int = 0
    var j:Int = 0
    for (k <- 0 until result.length) {
      if(i<first.length && j < second.length){
        if (first(i) < second(j)){
          result(k) = first(i)
          i=i+1
        } else {
          result(k) = second(j)
          j=j+1
        }
      }else if(i>=first.length && j<second.length){
        result(k) = second(j)
        j=j+1
      } else {
        result(k) = first(i)
        i=i+1
      }
    }
  }

  def printArray(array: Array[Int]) = {
    println(array.mkString(", "))
  }

  def main(args: Array[String]) {
   val p = scala.util.Random
    val ls = Random.shuffle((for (i <- 1 to 10) yield p.nextInt(100)).toList)

    var arr=ls.toArray
    println("original list " + arr.toList)
    sort(arr)
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
        sort(x)
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

    val file = new File("C:/Users/bindu/Desktop/Merge.csv")

    val bw = new BufferedWriter(new FileWriter(file))

    for (n <- 1 until time.length) {
      bw.write(mess(n).toString + "," + time(n).toString + "," + memory(n).toString + "\n")
    }
    bw.close()
  }
}
