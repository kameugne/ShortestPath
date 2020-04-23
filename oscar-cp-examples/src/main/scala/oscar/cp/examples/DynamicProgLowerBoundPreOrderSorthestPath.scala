package oscar.cp.examples


import java.io.{BufferedWriter, FileWriter}

import oscar.algo.DisjointSets

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

object DynamicProgLowerBoundPreOrderSorthestPath extends App{

  /*
  * Running example for the note
  * */
  val transition = GeneratePointsAndTransitionMatrix(4, -1, 3)
  val order = GeneratePreOrder(4, 50)
  val exactShortestPath = exactPremutationWithPreOrder(4 , order, transition)
  println(exactShortestPath)



  /*
  * Run the Easy instances of the problems
  */

  /*val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Easy/SPPWPEasySolution.txt"))
  writer.write("Instance |" + " nNode |" + "orderL| " + " MstLB| " + " DPLB |" + " bfDPLB |" + " PermLB|"+ " MstTime| " + " DPTime |" + " bfDPTime |" + " PermTime|")
  writer.newLine()
  for(tot <- List(6,7,8,9,10)){
    for(perc <- List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Easy/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode -1
      var order: Seq[Int] = data.order
      val transition = data.transition
      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val lowerBoundDynProg = dynamicProgramShortestPathOrderComplet(num, order, transition)
      val lowerBoundBruteForce = bruteForceShortestPathOrderComplet(num, order, transition)
      val exactShortestPath = exactPremutationWithPreOrder(num , order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +
        kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 + "| " + lowerBoundBruteForce._1 +  " | " + exactShortestPath._1 + " | " +
        kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 + " | " + lowerBoundBruteForce._2 + " | "+ exactShortestPath._2 + " | ")
      writer.write("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +
        kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 + "| " + lowerBoundBruteForce._1 +" | " + exactShortestPath._1 + " | " +
        kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 + "| " + lowerBoundBruteForce._2 + " | " + exactShortestPath._2 + "| ")
      writer.newLine()


    }
  }
  writer.close()*/


  /*
  * Run the Medium instances of the problems
  */

  /*val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Medium/SPPWPMediumSolution.txt"))
  writer.write("Instance |" + " nNode |" + "orderL| " + " MstLB| " + " DPLB |" + " MstTime| " + " DPTime |" )
  writer.newLine()
  for(tot <- List(15,20,25,30,35, 40)){
    for(perc <- List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Medium/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode -1
      var order: Seq[Int] = Seq().empty
      if(data.lengthOfOrder > 0)
        order = data.order
      val notInsert = data.notInOrder
      val transition = data.transition
      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val lowerBoundDynProg = dynamicProgramShortestPathOrderComplet(num, order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | "  +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 +  " | ")
      writer.write("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | " +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 + " | " )
      writer.newLine()


    }
  }
  writer.close()*/

  /*
  * Run the Hard instances of the problems
  */

  /*val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Hard/SPPWPHardSolution.txt"))
  writer.write("Instance |" + " nNode |" + "orderL| " + " MstLB| " + " DPLB |" + " MstTime| " + " DPTime |" )
  writer.newLine()
  for(tot <- List(50,60,70,80,90,100)){
    for(perc <- List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Hard/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode -1
      val order: Seq[Int] = data.order
      val transition = data.transition
      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val lowerBoundDynProg = dynamicProgramShortestPathOrderComplet(num, order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | "  +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 +  " | ")
      writer.write("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | " +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 + " | " )
      writer.newLine()


    }
  }
  writer.close()*/


  /*
  *  Dynamic program for the computation of a lower bound of p-edges shortest path preserving the pre-order "order" starting at a giving node
  *  (here 0). Nodes are given in a complet un-directed weigthed graph descrived by the matrix named "transition". The complexity of this algorithm is
  * O(pn^4) since the algorithm "dynamicProgramShortestPathOrder" of complexity O(pn^3) is called n time (here p <= n is the length of the vector "order"
  * and n the total number of nodes of the graph ).
  */
  def dynamicProgramShortestPathOrderComplet(p: Int, order: Seq[Int], transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    for(u <- transition.indices; if u != 0)
      distance = Math.min(distance, dynamicProgramShortestPathOrder(p, u, order, transition))
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (distance, time)
  }

  /*
  *  Dynamic program for the computation of a lower bound of p-edges shortest path preserving the pre-order "order" starting at a giving node
  *  (here 0). Nodes are given in a complet un-directed weigthed graph descrived by the matrix named "transition". The complexity of this algorithme is
  * O(pn^3) where p <= n is the length of the vector "order" and n is the total number of nodes of the graph.
  */

  def dynamicProgramShortestPathOrder(p: Int, u: Int, order: Seq[Int], transition: Array[Array[Double]]): Double ={
    val n = transition.length
    val m = order.length
    val shortPath: Array[Array[Array[Double]]] = Array.ofDim[Double](n,p+1,m+1)
    for(e <- 0 to p){
      for(s <- 0 to m){
        for(i <- 0 until n) {
          shortPath(i)(e)(s) = Double.MaxValue
          if(s == 0 && e == 0 && i == 0)
            shortPath(i)(e)(s) = 0
          if(s > 0){
            if(e == s && i == order(s-1))
              shortPath(i)(e)(s) = pathLength(order.toList.dropRight(m-s), transition)
            if(e > s && i != 0){
              val orderMinus = order.dropRight(m-s)
              if(orderMinus.contains(i)){
                if(i == orderMinus.last){
                  val orderM = orderMinus.dropRight(1)
                  for (a <- 0 until n if !order.contains(a) || ( orderM.nonEmpty && orderM.contains(a) && a == orderM.last)) {
                    if (a != 0 && a != i && shortPath(a)(e - 1)(s - 1) != Double.MaxValue) {
                      shortPath(i)(e)(s) = Math.min(shortPath(i)(e)(s), shortPath(a)(e - 1)(s - 1) + transition(a)(i))
                    }
                  }
                }
              }else{
                for (a <- 0 until n if !order.contains(a) || (orderMinus.nonEmpty && orderMinus.contains(a) && a == orderMinus.last)) {
                  if (a != 0 && a != i && shortPath(a)(e - 1)(s) != Double.MaxValue) {
                    shortPath(i)(e)(s) = Math.min(shortPath(i)(e)(s), shortPath(a)(e - 1)(s) + transition(a)(i))
                  }
                }
              }
            }
          }
        }
      }
    }
    shortPath(u)(p)(m)
  }



  /*
  *  Brute force program for the computation of a lower bound of p-edges shortest path preserving the pre-order "order" starting at a giving node
  *  (here 0). Nodes are given in a complet un-directed weigthed graph descrived by the matrix named "transition". The complexity of this algorithm is
  * O(n^(p+1)) since the algorithm "bruteForceShortestPathOrder" of complexity O(n^p) is called n time (here p <= n is the length of the vector "order"
  * and n the total number of nodes of the graph ).
  */


  def bruteForceShortestPathOrderComplet(p: Int, order: Seq[Int], transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    for(u <- transition.indices; if u != 0)
      distance = Math.min(distance, bruteForceShortestPathOrder(p, u, order, order, transition))
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (distance, time)

  }

  /*
  *  Brute force program (based on dynamic program) for the computation of a lower bound of p-edges shortest path preserving the pre-order "order" starting at a giving node
  *  (here 0). Nodes are given in a complet un-directed weigthed graph descrived by the matrix named "transition". The complexity of this algorithme is
  * O(n^p) where p <= n is the length of the vector "order" and n is the total number of nodes of the graph.
  */

  def bruteForceShortestPathOrder(p: Int, u: Int, order: Seq[Int], fixorder: Seq[Int], transition: Array[Array[Double]]): Double ={
    val n = transition.length
    val m = order.length
    if(u == 0 && p == 0)
      return  0
    if(u == 0 && p > 0)
      return Double.MaxValue
    if(p < m)
      return  Double.MaxValue
    if(p == m && u != order.last)
      return Double.MaxValue
    if(m == 0 && u != 0 && p == 0)
      return  Double.MaxValue
    var dist = Double.MaxValue
    if(m > 0){
      if(p == m && u == order.last)
        dist = Math.min(dist, pathLength(order.toList, transition))
      if(p > m && u != 0){
        if(u == order.last){
          val orderMinus = order.dropRight(1)
          for(a <- 0 until n if !fixorder.contains(a) || (orderMinus.nonEmpty && orderMinus.contains(a) && a == orderMinus.last)){
            if(a != 0 && a != u && bruteForceShortestPathOrder(p-1, a, orderMinus, fixorder,  transition) != Double.MaxValue){
              dist = Math.min(dist, bruteForceShortestPathOrder(p-1, a, orderMinus, fixorder, transition) + transition(a)(u))
            }
          }
        }else{
          if(!order.contains(u)){
            for(a <- 0 until n if !fixorder.contains(a) || (order.contains(a) && a == order.last)){
              if(a != 0 && a != u && bruteForceShortestPathOrder(p-1, a, order, fixorder, transition) != Double.MaxValue){
                dist = Math.min(dist, bruteForceShortestPathOrder(p-1, a, order, fixorder, transition) + transition(a)(u))
              }
            }
          }
        }
      }
    }
    dist
  }

  /*
   * Kruskal algorithm for the compuation of the minimum spanning tree of a graph. The MST is usually used as a lower bound a n-edges shortest path of
   * a graph of n+1 nodes. The program uses the priority queue for a complexity of O(n^2log n).
   */
  def KruskalMST(num: Int, transition: Array[Array[Double]]): (Double, Double) = {
    val startTime = System.currentTimeMillis()
    val nodes = 0 to num
    val edges = for(i <- nodes; j <- nodes if i != j) yield (i, j)
    implicit object EdgeOrdering extends Ordering[(Int, Int)] {
      override def compare(x: (Int, Int), y: (Int, Int)): Int = transition(y._1)(y._2) compare transition(x._1)(x._2)
    }
    val pq = mutable.PriorityQueue(edges: _*)
    val uf = new DisjointSets(0, nodes.length)
    val nodeIndex = nodes.zipWithIndex.toMap
    val mst = mutable.ArrayBuffer[(Int, Int)]()
    var weight: Double = 0

    while(pq.nonEmpty && mst.length < nodes.length - 1){
      val (i, j) = pq.dequeue()
      val iIdx = nodeIndex(i)
      val jIdx = nodeIndex(j)
      if(!uf.inSameSet(iIdx, jIdx)){
        uf.union(iIdx, jIdx)
        mst += ((i, j))
        weight += transition(i)(j)
      }
    }
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/100
    (weight, time)
  }

  /*
   * GeneratePreOrder: Given a number of requiered and a percentatge of nodes already insert in the sequence,
   * a random pre-order vector is generated
   */
  def GeneratePreOrder(num: Int, perc: Int): Seq[Int] = {
    var initialVector = (1 to num).toArray
    val numNodeToGenerate = Math.ceil((num * perc)/100).toInt
    val preOrder = new Array[Int](numNodeToGenerate)
    val rand = new Random(1L)
    var i = 0
    while(i < numNodeToGenerate){
      val index = rand.nextInt(initialVector.length)
      if(index == num-1){
        preOrder(i) = initialVector(index)
        initialVector = initialVector.dropRight(1)
      }else{
        val temp = initialVector(index)
        preOrder(i) = temp
        initialVector(index) = initialVector(initialVector.length-1)
        initialVector(initialVector.length-1) = temp
        initialVector = initialVector.dropRight(1)
      }
      i += 1
    }

    preOrder.toSeq
  }



  /*
   * Generates n different points with integers coordinates picked between min and max. The points are different to the origin supposes to be
   * the bottom position. The transition matrix is also built and return.
   */

  def GeneratePointsAndTransitionMatrix(n: Int, min: Int, max: Int): Array[Array[Double]] = {
    val matrix: Array[Array[Double]] = Array.ofDim[Double](n+1,n+1)
    val rand = new scala.util.Random(1L)
    val points: ArrayBuffer[(Int,Int)] = ArrayBuffer[(Int, Int)]()
    var i = 0
    while(i < n){
      val currentPoint = (min + rand.nextInt(max), min + rand.nextInt(max))
      if(isNotAlreadyGenerate(currentPoint, points.toArray)){
        points += currentPoint
        i += 1
      }
    }
    println(points.mkString(" - "))
    for(i <- 0 until n+1; j <- 0 until n+1){
      if(i == 0 && j != 0) {
        matrix(i)(j) = Math.sqrt(Math.pow(points(j-1)._1, 2) + Math.pow(points(j-1)._2, 2))
      }
      if(j == 0 && i != 0){
        matrix(i)(j) = Math.sqrt(Math.pow(points(i-1)._1, 2) + Math.pow(points(i-1)._2, 2))
      }
      matrix(0)(0) = 0
      if(i != 0 && j != 0){
        matrix(i)(j) = Math.sqrt(Math.pow(points(i-1)._1 - points(j-1)._1, 2) + Math.pow(points(i-1)._2 - points(j-1)._2, 2))
      }
    }
    matrix
  }

  def GeneratePoints(n: Int, min: Int, max: Int): Array[(Int,Int)] = {
    val rand = new scala.util.Random()
    val points: ArrayBuffer[(Int, Int)] = ArrayBuffer[(Int, Int)]()
    var i = 0
    while (i < n) {
      val currentPoint = (min + rand.nextInt(max), min + rand.nextInt(max))
      if (isNotAlreadyGenerate(currentPoint, points.toArray)) {
        points += currentPoint
        i += 1
      }
    }
    points.toArray
  }

  /*
  * Check if the new point is different to already generated points and to the origin.
  */
  def isNotAlreadyGenerate(currentPoint: (Int, Int), points: Array[(Int, Int)]): Boolean ={
    if(points.exists(p => (p._1 == currentPoint._1 && p._2 == currentPoint._2) || (currentPoint._1 == 0 && currentPoint._1 == 0)))
      return false
    true
  }
  /*
  * Exact program that built the path of minimum length with pre-order. The permutation of minimum length if returned.
  */

  def exactPremutationWithPreOrder(num: Int, preOrder: Seq[Int], transition: Array[Array[Double]]): (Double, Double)={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    var bestPerm = (1 to num).toList
    for(perm <- (1 to num).toList.permutations){
      //println(perm.mkString(" - "))
      if(preOrder.nonEmpty){
        val pos = preOrder.map(o => perm.indexOf(o))
        if((0 until (pos.length-1)).forall(i => pos(i) < pos(i+1)) && pathLength(perm, transition) < distance){
          distance = Math.min(distance, pathLength(perm, transition))
          bestPerm = perm
        }
      }else{
        if(pathLength(perm, transition) < distance){
          distance = Math.min(distance, pathLength(perm, transition))
          bestPerm = perm
        }
      }
    }
    val endTime = System.currentTimeMillis()
    val time  = (endTime - startTime)/100
    println(bestPerm.mkString(" - ") + "      " +preOrder.mkString(" - "))
    (distance, time)
  }

  /*
  * Length of the path starting at the bottom position
  */

  def pathLength(perm: List[Int], transition: Array[Array[Double]]): Double ={
    var score: Double = 0
    for(i <- 0 until perm.length){
      if(i == 0)
        score += transition(0)(perm(0))
      else
        score += transition(perm(i-1))(perm(i))
    }
    score
  }

  def InstancesGenerator(num: Int, perc: Int): Unit = {
    val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Hard/SPPWP_"
      + num + "_"+((perc*num)/100)+".txt"))
    println(num+1 + " "+ ((perc*num)/100))
    writer.write(num+1 + " "+ ((perc*num)/100))
    writer.newLine()
    if(((perc*num)/100) > 0) {
      for (i <- GeneratePreOrder(num, perc)) {
        print(i + " ")
        writer.write(i + " ")
      }
    }
    writer.newLine()
    println("  ")
    for(i <- 0 to num) {
      if (i == 0) {
        println(0 + " " + 0)
        writer.write(0 + " " + 0)
        writer.newLine()
      }
      else{
        writer.write(GeneratePoints(num, -10*num, 20*num+1)(i-1)._1 + " " + GeneratePoints(num, -10*num, 20*num+1)(i-1)._2)
        writer.newLine()
        println(GeneratePoints(num, -10*num, 20*num+1)(i-1)._1 + " " + GeneratePoints(num, -10*num, 20*num+1)(i-1)._2)
      }
    }
    writer.close()
  }

}
class SPPWPInstance(val nNode: Int, val lengthOfOrder:Int, val notInOrder: Set[Int], val order: Seq[Int], val transition: Array[Array[Double]])

object SPPWPPaser{
  def paser(filepath: String): SPPWPInstance ={
    val lines = Source.fromFile(filepath).getLines
    val line1 = lines.next.trim.split("[ ,\t]+")
    val nNode = line1(0).toInt
    val lengthOfOrder = line1(1).toInt
    val order: ArrayBuffer[Int] = ArrayBuffer[Int]()
    val line = lines.next.trim.split("[ ,\t]+")
    if(lengthOfOrder > 0){
      for(i <- 0 until lengthOfOrder)
        order += line(i).toInt
    }
    val coordBf = ArrayBuffer[(Int, Int)]()
    while(lines.hasNext){
      val line = lines.next
      if (!line.isEmpty) {
        val data = line.trim.split("[ ,\t]+")
        coordBf += ((data(0).toInt, data(1).toInt))
      }
    }
    val transition: Array[Array[Double]] = Array.ofDim[Double](coordBf.length, coordBf.length)
    for(i <- 0 until coordBf.length; j <- 0 until coordBf.length){
      transition(i)(j) = Math.sqrt(Math.pow(coordBf(i)._1 - coordBf(j)._1, 2) + Math.pow(coordBf(i)._2 - coordBf(j)._2, 2))
    }
    val notInOrder = (1 until nNode).toSet.diff(order.toSet)
    new SPPWPInstance(nNode, lengthOfOrder, notInOrder, order.toSeq, transition)
  }



}

