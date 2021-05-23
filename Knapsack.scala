object Knapsack {
  
  def solve_knapsack(profits:List[Int],weights:List[Int],capacity:Int):Int ={
   return knapsack_recursive(profits, weights, capacity, 0)
  }                                               //> solve_knapsack: (profits: List[Int], weights: List[Int], capacity: Int)Int
  
  def knapsack_recursive(profits:List[Int],weights:List[Int],capacity:Int,currentIndex:Int): Int ={
  	// base checks
  	
  	if (capacity<=0 || currentIndex >= profits.length) 	return 0
  	//recursive call after choosing the element at the currentIndex
    // if the weight of the element at currentIndex exceeds the capacity, we  shouldn't process this
  	var profit1 = 0
  	
  	if (weights(currentIndex) <= capacity){
  	 		profit1 = profits(currentIndex) + knapsack_recursive(profits, weights, capacity - weights(currentIndex), currentIndex + 1)
  	}
  
  // recursive call after excluding the element at the currentIndex
  var profit2 = knapsack_recursive(profits, weights, capacity, currentIndex + 1)

  return (profit1.max(profit2))
  
  }                                               //> knapsack_recursive: (profits: List[Int], weights: List[Int], capacity: Int, 
                                                  //| currentIndex: Int)Int
  
  solve_knapsack(List(1, 6, 10, 16), List(1, 2, 3, 5), 7)
                                                  //> res0: Int = 22
  
  }
