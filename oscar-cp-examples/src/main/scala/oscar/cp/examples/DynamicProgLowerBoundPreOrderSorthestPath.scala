package oscar.cp.examples


import java.io.{BufferedWriter, FileWriter}

import oscar.algo.DisjointSets

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

object DynamicProgLowerBoundPreOrderSorthestPath extends App{
  for(tot <- List(6,7,8,9,10)){//List(15,20,25,30,35, 40)){
    for (perc <- List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Easy/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode - 1
      var order: Seq[Int] = Seq().empty
      if (data.lengthOfOrder > 0)
        order = data.order
      val notInsert = data.notInOrder
      val transition = data.transition
      val exactShortestPath = exactPremutationWithPreOrder(num , order, transition)
      val bruteforce = bruteForceShortestPathComplet(num,  transition)
      val dynamicProgam = dynamicProgramShortestPathComplet(num, transition)
      val bruteforceOrder = bruteForceShortestPathOrderComplet(num, order, transition)
      val dynamicProgramOrder =dynamicProgramShortestPathOrderComplet(num, order, transition)
      val mstShortestPath = KruskalMST(num, transition)
      //println("bf: "+  bruteforce._1 +  "       dp: " + dynamicProgam._1 + "      bfO: " + bruteforceOrder._1  + "       dpO: " + dynamicProgramOrder._1+ "         exact: "+ exactShortestPath)
      //println(bruteforce +  "       " + dynamicProgam + "      " + bruteforceOrder + "         "+ exactShortestPath)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  mstShortestPath._1 + " | " + dynamicProgramOrder._1 + " | "+ + bruteforceOrder._1
        + " | " + exactShortestPath._1
        + " | "  +  mstShortestPath._2 + " | " + dynamicProgramOrder._2 + " | " + bruteforceOrder._2 + " | " + exactShortestPath._2 + " | ")
    }
  }



  /*val transition = GeneratePointsAndTransitionMatrix(4, -1, 2)
  val bfOrder = bruteForceShortestPathOrder(5, 5, Seq(2,1), transition)
  val bf = bruteForceShortestPath(3, 2, transition)
  val dp = dynamicProgramShortestPath(3, 2, transition)
  val dpOrder = dynamicProgramShortestPathOrder(5, 5, Seq(2,1), transition)
  println(bfOrder + "  " + bf + "    "+dp + "      "+ dpOrder)*/
  def dynamicProgramShortestPathOrderComplet(p: Int, order: Seq[Int], transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    for(u <- transition.indices; if u != 0)
      distance = Math.min(distance, dynamicProgramShortestPathOrder(p, u, order, transition))
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (distance, time)
  }

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
          if(s == 0 && e > 0 && i != 0)
            shortPath(i)(e)(s) = dynamicProgramShortestPath(e, i, transition)
          if(s > 0){
            if(e == s && i == order(s-1))
              shortPath(i)(e)(s) = pathLength(order.toList.dropRight(m-s), transition)
            if(e > s){
              if(i == 0)
                shortPath(i)(e)(s) = 0
              if(order.contains(i)){
                if(order.indexOf(i) >= s-1){
                  for (a <- 0 until n) {
                    if (a != 0 && a != i && shortPath(a)(e - 1)(s - 1) != Double.MaxValue) {
                      shortPath(i)(e)(s) = Math.min(shortPath(i)(e)(s), shortPath(a)(e - 1)(s - 1) + transition(a)(i))
                    }
                  }
                }
              }else{
                for (a <- 0 until n) {
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
  * Brute force dynamic program for the k-edges shortest path problem with pre-order.
  */

  def bruteForceShortestPathOrderComplet(p: Int, order: Seq[Int], transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    for(u <- transition.indices; if u != 0)
      distance = Math.min(distance, bruteForceShortestPathOrder(p, u, order, transition))
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (distance, time)

  }


  def bruteForceShortestPathOrder(p: Int, u: Int, order: Seq[Int], transition: Array[Array[Double]]): Double ={
    val n = transition.length
    val m = order.length
    if(u == 0)
      return  0
    if(p < order.length)
      return  Double.MaxValue
    if(p == order.length && u == order.last)
      return pathLength(order.toList, transition)
    if(p <= 0)
      return Double.MaxValue
    if(order.contains(u) && u != order.last)
      return Double.MaxValue
    if(order.isEmpty)
      return  bruteForceShortestPath(p, u, transition)
    var dist = Double.MaxValue
    if(u == order.last){
      val orderMinus = order.dropRight(1)
      for(a <- 0 until n){
        if(a != 0 && a != u && bruteForceShortestPathOrder(p-1, a, orderMinus, transition) != Double.MaxValue){
          dist = Math.min(dist, bruteForceShortestPathOrder(p-1, a, orderMinus, transition) + transition(a)(u))
        }
      }
    }else{
      for(a <- 0 until n){
        if(a != 0 && a != u && bruteForceShortestPathOrder(p-1, a, order, transition) != Double.MaxValue){
          dist = Math.min(dist, bruteForceShortestPathOrder(p-1, a, order, transition) + transition(a)(u))
        }
      }
    }
    dist
  }

  def bruteForceShortestPathComplet(p: Int, transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    for(u <- transition.indices; if u != 0)
      distance = Math.min(distance, bruteForceShortestPath(p, u, transition))
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (distance, time)
  }


  def bruteForceShortestPath(p: Int, u: Int, transition: Array[Array[Double]]): Double ={
    val n = transition.length
    if(p == 0 && u == 0)
      return  0
    if(p == 1)
      return transition(0)(u)
    if(p <= 0)
      return Double.MaxValue
    var dist = Double.MaxValue
    for(i <- 0 until n){
      val cur_dist = bruteForceShortestPath(p-1, i, transition)
      if(i != 0 && i != u && cur_dist != Double.MaxValue){
        dist = Math.min(dist, cur_dist + transition(i)(u))
      }
    }
    dist
  }


  def dynamicProgramShortestPathComplet(p: Int, transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var distance = Double.MaxValue
    for(u <- transition.indices; if u != 0)
      distance = Math.min(distance, dynamicProgramShortestPath(p, u, transition))
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (distance, time)
  }


  def dynamicProgramShortestPath(p: Int, u: Int, transition: Array[Array[Double]]): Double ={
    val n = transition.length
    val shortPath: Array[Array[Double]] = Array.ofDim[Double](n,p+1)
    for(e <- 0 to p){
      for(i <- 0 until n){
        shortPath(i)(e) = Double.MaxValue
        if(e == 0 && i == 0)
          shortPath(i)(e) = 0
        if(e == 1)
          shortPath(i)(e) = transition(0)(i)
        if(e > 1){
          for(a <- 0 until n){
            if(a != 0 && a != i && shortPath(a)(e-1) != Double.MaxValue){
              shortPath(i)(e) = Math.min(shortPath(i)(e), shortPath(a)(e-1)+transition(a)(i))
            }
          }
        }
      }
    }
    shortPath(u)(p)
  }










  /*
   * LowerBound of the shortest path starting from the bottom node (origin) that satisfy the pre-order.
   */
  def LowerBoundShortestPathDynProg(k: Int, preOrder: Seq[Int], transition: Array[Array[Double]]): (Double, Double) ={
    val startTime = System.currentTimeMillis()
    var lowerBoundShortestPath = DynamicProgEndsInPreOrderB(k, 0, preOrder, transition)
    for(j <- (1 until transition.length).toSet.diff(preOrder.toSet)){
      lowerBoundShortestPath = Math.min(lowerBoundShortestPath, DynamicProgEndsOutOfPreOrderB(k, 0, j, preOrder, transition))
    }
    val endTime = System.currentTimeMillis()
    val time = (endTime - startTime)/1000
    (lowerBoundShortestPath, time)
  }







  /*
  * A dynamic program for shortes path of k-edges with n vertices without pre-order in O(kn^3)
  */

  def DynamicProgShortestPathBest(k: Int, s: Int, v: Int, transition: Array[Array[Double]]): Double = {
    val n = transition.length
    val sp: Array[Array[Array[Double]]] =  Array.ofDim[Double](n,n,k+1)
    for(nEdge <- 0 to k){
      for(sour <- 0 until n){
        for(dest <- 0 until n){
          sp(sour)(dest)(nEdge) = Double.MaxValue
          if (nEdge == 0 && sour == dest)
            sp(sour)(dest)(nEdge) = 0
          if (nEdge == 1)
            sp(sour)(dest)(nEdge) = transition(sour)(dest)
          if(nEdge > 1)
          {
            for (a <- 0 until n) {
              if (sour != a && dest != a && sp(sour)(dest)(nEdge-1) != Double.MaxValue)
                sp(sour)(dest)(nEdge) = Math.min(sp(sour)(dest)(nEdge), transition(sour)(a) + sp(a)(dest)(nEdge-1))
            }
          }
        }
      }
    }
    sp(s)(v)(k)
  }



  /*
   * Dynamic program for the shortest path of with no pre-order. DP(k,seq(A)) is the length of shortest path of
   * k edges, started at the bottom position (origine point (0,0)) and ends at the last node of the pre-order
   */

  def DynamicProgEndsInPreOrderB(k: Int, s: Int, preOrder: Seq[Int], transition: Array[Array[Double]]): Double = {
    val n = transition.length
    val sp: Array[Array[Array[Array[Double]]]] =  Array.ofDim[Double](n,n,k+1, preOrder.length+1)
    for(nEdge <- 0 to k){
      for(ord <- 0 to preOrder.length){
        for(sour <- 0 until n) {
          for(dest <- 0 until n) {
            sp(sour)(dest)(nEdge)(ord) = Double.MaxValue
            if(nEdge == 0 || sour == dest || ord == 0)
              sp(sour)(dest)(nEdge)(ord) = 0
            if (nEdge < ord)
              sp(sour)(dest)(nEdge)(ord) = 0
            if(nEdge == ord)
              sp(sour)(dest)(nEdge)(ord) = pathLength(preOrder.toList.dropRight(preOrder.length - ord), transition)
            if (nEdge > ord) {
              if(ord == 1)
                sp(sour)(dest)(nEdge)(ord) = DynamicProgShortestPathBest(nEdge, s, preOrder(ord-1), transition)
              if(ord > 1){
                for (a <- 0 until n) {
                  val inOrder = preOrder(ord-1)
                  if(a == inOrder && sour != a && dest != a && sp(sour)(a)(nEdge - 1)(ord-1) != Double.MaxValue)
                    sp(sour)(dest)(nEdge)(ord) = Math.min(sp(sour)(dest)(nEdge)(ord), transition(a)(dest) + sp(sour)(a)(nEdge - 1)(ord-1))
                  if (a != inOrder && sour != a && dest != a && sp(sour)(a)(nEdge - 1)(ord-1) != Double.MaxValue && !preOrder.contains(a))
                    sp(sour)(dest)(nEdge)(ord) = Math.min(sp(sour)(dest)(nEdge)(ord), transition(a)(dest) + sp(sour)(a)(nEdge - 1)(ord-1))
                }
              }
            }
          }
        }
      }
    }
    sp(s)(preOrder.last)(k)(preOrder.length)
  }

  /*
   * Dynamic program for the shortest path of with no pre-order. DP(k,j,seq(A)) is the length of shortest path of
   * k edges, started at the bottom position (origine point (0,0)) and ends at node j where j is not member of the pre-order
   */

  def DynamicProgEndsOutOfPreOrderB(k: Int, s: Int, v: Int, preOrder: Seq[Int], transition: Array[Array[Double]]): Double ={
    val n = transition.length
    val sp: Array[Array[Array[Array[Double]]]] =  Array.ofDim[Double](n,n,k+1, preOrder.length+1)
    for(nEdge <- 0 to k){
      for(ord <- 0 to preOrder.length){
        for(sour <- 0 until n) {
          for(dest <- 0 until n) {
            sp(sour)(dest)(nEdge)(ord) = Double.MaxValue
            if(nEdge == 0 || sour == dest)
              sp(sour)(dest)(nEdge)(ord) = 0
            if(ord == 0)
              sp(sour)(dest)(nEdge)(ord) = DynamicProgShortestPathBest(nEdge, s, v, transition)
            if (nEdge <= ord)
              sp(sour)(dest)(nEdge)(ord) = 0
            if(nEdge == ord+1 && ord > 0)
              sp(sour)(dest)(nEdge)(ord) = pathLength(preOrder.toList.dropRight(preOrder.length - ord), transition) + transition(preOrder(ord-1))(v)
            if (nEdge > ord+1 && ord > 0) {
              for (a <- 0 until n) {
                val inOrder = preOrder(ord-1)
                if(a == inOrder && sour != a && dest != a && sp(sour)(a)(nEdge-1)(ord) != Double.MaxValue)
                  sp(sour)(dest)(nEdge)(ord) = Math.min(sp(sour)(dest)(nEdge)(ord), transition(a)(dest) + sp(sour)(a)(nEdge - 1)(ord))
                if (a != inOrder && sour != a && dest != a && sp(sour)(a)(nEdge - 1)(ord) != Double.MaxValue && !preOrder.contains(a))
                  sp(sour)(dest)(nEdge)(ord) = Math.min(sp(sour)(dest)(nEdge)(ord), transition(a)(dest) + sp(sour)(a)(nEdge - 1)(ord))
              }
            }
          }
        }
      }
    }
    sp(s)(preOrder.last)(k)(preOrder.length)
  }



  /*
   * Kruskal MST
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
    var seed: Long = 0
    var initialVector = (1 to num).toArray
    val numNodeToGenerate = Math.ceil((num * perc)/100).toInt
    val preOrder = new Array[Int](numNodeToGenerate)
    val rand = new Random()
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
    val rand = new scala.util.Random(1)
    val points: ArrayBuffer[(Int,Int)] = ArrayBuffer[(Int, Int)]()
    var i = 0
    while(i < n){
      val currentPoint = (min + rand.nextInt(max), min + rand.nextInt(max))
      if(isNotAlreadyGenerate(currentPoint, points.toArray)){
        points += currentPoint
        i += 1
      }
    }
    //println(points.mkString(" - "))
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
    if(points.exists(p => (p._1 == currentPoint._1 && p._2 == currentPoint._2) || (p._1 == 0 && p._2 == 0)))
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
    //println(bestPerm.mkString(" - ") + "      " +preOrder.mkString(" - "))
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

  /*val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Easy/SPPWPEasySolution.txt"))
  writer.write("Instance |" + " nNode |" + "orderL| " + " MstLB| " + " DPLB |" + " PermLB|"+ " MstTime| " + " DPTime |" + " PermTime|")
  writer.newLine()
  for(tot <- List(6,7,8,9,10)){
    for(perc <- List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Easy/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode -1
      var order: Seq[Int] = Seq().empty
      if(data.lengthOfOrder > 0)
        order = data.order
      val notInsert = data.notInOrder
      val transition = data.transition
      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val lowerBoundDynProg = LowerBoundShortestPathDynProg(num, notInsert, order, transition)
      val exactShortestPath = exactPremutationWithPreOrder(num , order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | " + exactShortestPath._1 + " | " +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 +
        " | " + exactShortestPath._2 + " | ")
      writer.write("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | " + exactShortestPath._1 + " | " +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 +
        " | " + exactShortestPath._2 + " | ")
      writer.newLine()


    }
  }
  writer.close()*/



  /*val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Medium/SPPWPMediumSolution.txt"))
  writer.write("Instance |" + " nNode |" + "orderL| " + " MstLB| " + " DPLB |" + " MstTime| " + " DPTime |" )
  writer.newLine()
  for(tot <- List(40)){//List(15,20,25,30,35, 40)){
    for(perc <- List(80).map(i => tot*i/100)){//List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Medium/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode -1
      var order: Seq[Int] = Seq().empty
      if(data.lengthOfOrder > 0)
        order = data.order
      val notInsert = data.notInOrder
      val transition = data.transition
      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val lowerBoundDynProg = LowerBoundShortestPathDynProg(num, notInsert, order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | "  +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 +  " | ")
      writer.write("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | " +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 + " | " )
      writer.newLine()


    }
  }
  writer.close()*/



  /*val writer: BufferedWriter = new BufferedWriter(new FileWriter("Data/ShortestPathWithPreOrder/Hard/SPPWPHardSolution.txt"))
  writer.write("Instance |" + " nNode |" + "orderL| " + " MstLB| " + " DPLB |" + " MstTime| " + " DPTime |" )
  writer.newLine()
  for(tot <- List(50,60,70,80,90,100)){
    for(perc <- List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Hard/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode -1
      var order: Seq[Int] = Seq().empty
      if(data.lengthOfOrder > 0)
        order = data.order
      val notInsert = data.notInOrder
      val transition = data.transition
      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val lowerBoundDynProg = LowerBoundShortestPathDynProg(num, notInsert, order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | "  +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 +  " | ")
      writer.write("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1  + " | " + lowerBoundDynProg._1 +
        " | " +  kruskalMSTLowerBound._2  + " | " + lowerBoundDynProg._2 + " | " )
      writer.newLine()


    }
  }
  writer.close()*/


  /*val listOfNumWithExact = List(50,60,70,80,90,100)
  val listOfPercentage = List(40,50,60,70,80)
  println("num |"+ " prec|" + " Kruskal|" + " DP |" + " Exact|")
  for(num <- listOfNumWithExact){
    for(perc <- listOfPercentage){
      InstancesGenerator(num,perc)
    }
  }*/

  /*
  * Dynamic program for the shortest path of without pre-order. DP(k,j,A) is length of the shortest path of
  * k edges, started at the bottom position (origine point (0,0)) and ends at point j
  */
  /*def DynamicProgShortestPath(k: Int, node: Int, set: Set[Int], transition: Array[Array[Double]]): Double = {
    var distance = Double.MaxValue
    if(k == 0)
      0
    else{
      if(k == 1)
        Math.min(distance, transition(0)(node))
      else{
        val setMinus = set.diff(Set(node))
        for (j <- setMinus) {
          distance = Math.min(distance, DynamicProgShortestPath(k - 1, j, setMinus.diff(Set(j)), transition) + transition(j)(node))
        }
        distance
      }
    }
  }*/


  /*def DynamicProgShortestPathNaive(k: Int, s: Int, v: Int, transition: Array[Array[Double]]): Double = {
    var distance = Double.MaxValue
    if (k == 0 && s == v)
      0
    if(k == 1 && s != v)
      transition(s)(v)
    if(k <= 0)
      Double.MaxValue
    for(j <- transition.indices){
      if(j != s && j != v){
        val dist = DynamicProgShortestPathNaive(k-1,j,v,transition)
        if(dist != Double.MaxValue)
          distance = Math.min(distance, dist + transition(s)(j))
      }
    }
    distance
  }*/


  /*for(tot <- List(6)){//List(6,7,8,9,10)){//List(15,20,25,30,35, 40)){
    for (perc <- List(40).map(i => tot*i/100)){//List(40,50,60,70,80).map(i => tot*i/100)){
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Easy/SPPWP_" + tot + "_" + perc + ".txt")
      val num = data.nNode - 1
      var order: Seq[Int] = Seq().empty
      if (data.lengthOfOrder > 0)
        order = data.order
      val notInsert = data.notInOrder
      val transition = data.transition
      val exactShortestPath = exactPremutationWithPreOrder(num , order, transition)
      val bruteforce = bruteForceShortestPath(num, num,  transition)
      val dynamicProgam = dynamicProgramShortestPath(num, num, transition)
      val bruteforceOrder = bruteForceShortestPathOrder(num, num, order, transition)
      val dynamicProgramOrder =dynamicProgramShortestPathOrder(num, num, order, transition)
      println("bf: "+  bruteforce +  "       dp: " + dynamicProgam + "      bfO: " + bruteforceOrder  + "       dpO: "+ dynamicProgramOrder+ "         "+ exactShortestPath)
      //println(bruteforce +  "       " + dynamicProgam + "      " + bruteforceOrder + "         "+ exactShortestPath)




      val kruskalMSTLowerBound = KruskalMST(num, transition)
      val exactShortestPath = exactPremutationWithPreOrder(num , order, transition)
      val shortestpathDynProg = LowerBoundShortestPathDynProg(num, order, transition)
      println("SPPWP_" + tot + "_" + perc + " | " + num + " | " + perc + " | " +  kruskalMSTLowerBound._1 + " | " + shortestpathDynProg._1
        + " | " + exactShortestPath._1
        + " | "  +  kruskalMSTLowerBound._2 + " | " + shortestpathDynProg._2
        + " | " + exactShortestPath._2 +
        " | ")
    }
  }*/

}

