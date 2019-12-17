package oscar.cp.searches.lns.operators

import oscar.algo.Inconsistency
import oscar.algo.reversible.ReversibleInt
import oscar.cp.{CPIntVar, CPSolver}
import oscar.cp.searches.lns.CPIntSol

import scala.collection.mutable
import scala.util.Random

/**
  * This class keeps track of the closeness relationship between variables. The closeness is defined as the average
  * volume of propagation that is involved on one variable when another is instantiated.
  */
class PropagationGuidedRelax(variables: Seq[CPIntVar]){
  val nVars: Int = variables.length

  //Data structure to keep track of domain size:
  val startDomainSize: Array[Int] = Array.tabulate(nVars)(i => variables(i).size)
  val prevDomainSize: Array[ReversibleInt] = Array.tabulate(nVars)(i => ReversibleInt(variables(i).size)(variables(i).store))

  //Data structure to keep track of closeness:
  //TODO: If too much variables, these arrays are way to big! => Use better structure
  val nImpacted: Array[Array[Int]] = Array.fill(nVars, nVars)(0)
  val closeness: Array[Array[Double]] = Array.tabulate(nVars, nVars){ (i, j) => if(i == j) 1.0 else 0.0}

  /**
    * Updates the closeness store with the propagation given an instantiated variable.
    * @param instantiated the var that has been instantiated.
    */
  //TODO: automatically call this when assigning var in search (use listener?)
  def updateCloseness(instantiated: Int): Unit ={
    for(i <- variables.indices) if(i != instantiated){
      val currentDomSize = variables(i).size

      //If domain size has changed:
      if(currentDomSize < prevDomainSize(i).value){
        val impact = (prevDomainSize(i) - currentDomSize).toDouble / startDomainSize(i)

        //Updating closeness:
        closeness(instantiated)(i) = (closeness(instantiated)(i) * nImpacted(instantiated)(i) + impact) / (nImpacted(instantiated)(i) + 1)
        nImpacted(instantiated)(i) +=1

        prevDomainSize(i).setValue(currentDomSize)
      }
    }
//    println("closeness:\n" + closeness.map(_.mkString(", ")).mkString("\n"))
  }

  /**
   * Updates the closeness store with the propagation given an instantiated variable.
   * @param instantiated the var that has been instantiated.
   * @return the index of the most impacted unbound variable (-1 if no variable has been impacted)
   */
  def updateClosenessAndGetMostImpacted(instantiated: Int): Int ={
    var maxImpact = 0.0
    var mostImpacted = -1

    for(i <- variables.indices) if(i != instantiated){
      val currentDomSize = variables(i).size

      //If domain size has changed:
      if(currentDomSize < prevDomainSize(i).value){
        val impact = (prevDomainSize(i) - currentDomSize).toDouble / startDomainSize(i)

        //Updating closeness:
        closeness(instantiated)(i) = (closeness(instantiated)(i) * nImpacted(instantiated)(i) + impact) / (nImpacted(instantiated)(i) + 1)
        nImpacted(instantiated)(i) +=1

        //Checking impact:
        if(!variables(i).isBound && impact > maxImpact){
          mostImpacted = i
          maxImpact = impact
        }

        prevDomainSize(i).setValue(currentDomSize)
      }
    }
//    println("closeness:\n" + closeness.map(_.mkString(", ")).mkString("\n"))

    mostImpacted
  }

  /**
    * Get the closeness between vars i and j.
    * @return the closeness between i and j or 0.0 if i or j are not valid indices.
    */
  def getCloseness(i:Int, j:Int): Double = if(i < nVars && j < nVars) closeness(i)(j) else 0.0

  /**
    * Get the close vars (closeness > 0) of i.
    * @return a list of the close vars of i (empty if no var is close or i is not a valid index).
    */
  def getClose(i: Int): Iterable[Int] ={
    var closeSet = new mutable.ListBuffer[Int]
    if(i >= nVars) return closeSet

    for(v <- closeness(i).indices) if(v != i && closeness(i)(v) > 0.0)
      closeSet += v

    closeSet
  }

  /**
    * returns the close subset to which initalVar is a part of.
    * @return all the vars in the subset excluding initial var (might be empty).
    */
  def getCloseSubset(initialVar: Int, maxSubsetSize: Int): mutable.LinkedHashSet[Int]= {
    val subset = new mutable.LinkedHashSet[Int]  //Current subset
    if(initialVar >= nVars) return subset

    val closeToSubset = Array.fill[Double](nVars){0.0}
    val toAddNext = new mutable.PriorityQueue[Int]()(Ordering.by[Int, Double](i => closeToSubset(i)))
    for(v <- getClose(initialVar)){
      closeToSubset(v) = closeness(initialVar)(v)
      toAddNext += v
    }

    while(subset.size < maxSubsetSize && toAddNext.nonEmpty){
      val v = toAddNext.dequeue()
      if(v != initialVar && !subset.contains(v)){
        subset += v
        for(x <- getClose(v)) if(!subset.contains(v) && closeness(v)(x) > closeToSubset(x)){
          closeToSubset(x) = closeness(v)(x)
          toAddNext += x
        }
      }
    }

    subset
  }

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def propagationGuidedRelax(solver: CPSolver, currentSol: CPIntSol, s: Double): Unit = {
    //    println("relaxing to size " + s)
    val varArray = Array.tabulate(variables.length)(i => i) //map to real index of variables
    var boundStart = varArray.length //Elements of varArray from this index are bound
    var size = variables.map(v => math.log(v.size)).sum //Current estimation of the search space obtained
    var toFreezeNext = -1 //next var to freeze (most impacted by previous propagation)

    while (size > s) {

      val next = if (toFreezeNext == -1) varArray(Random.nextInt(boundStart)) //If no var to freeze next, selecting random var
      else toFreezeNext
      solver.add(variables(next) === currentSol.values(next)) //Freezing var => propagation occurs
      // propagation should be called as var is frozen
      if (!variables(next).isBound) throw Inconsistency

      //Updating closeness and next var to freeze:
      toFreezeNext = updateClosenessAndGetMostImpacted(next)

      //Updating size and bounded vars:
      size = 0.0
      var i = 0
      while (i < boundStart) {
        val x = varArray(i)
        size += math.log(variables(x).size) //Updating size

        //Marking var as bound:
        if (variables(x).isBound) {
          boundStart -= 1
          varArray(i) = varArray(boundStart)
          varArray(boundStart) = x
        }
        else i += 1
      }
    }
//    println("relaxation done, " + (variables.length - boundStart) + " vars frozen")
  }

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def reversedPropagationGuidedRelax(solver: CPSolver, currentSol: CPIntSol, s: Double): Unit = {
    //    println("relaxing to size " + s)
    val varArray = Array.tabulate(variables.length)(i => i) //map to real index of variables

    var next = Random.nextInt(variables.length) //Selecting randomly first var to relax
    varArray(next) = variables.length-1
    varArray(variables.length-1) = next

    var relaxStart = varArray.length-1 //Elements of varArray from this index are part of the relaxed variables
    var avgSize = variables(next).size
    var size = math.log(avgSize) //Current estimation of the search space obtained
    var subset = getCloseSubset(next, (math.round(s - size) / avgSize).toInt) //Subset of next

    while (size < s && relaxStart > 0) {

      if(subset.isEmpty){ //No more element in subset:
        next = Random.nextInt(relaxStart) //Selecting new random var as next
        subset = getCloseSubset(next, 10) //Retrieving subset of this var
      }
      else{
        next = subset.head
        subset -= next
      }

      relaxStart -= 1
      varArray(next) = varArray(relaxStart)
      varArray(relaxStart) = next
      val relaxed = variables(next).size
      avgSize = (avgSize * (variables.length - relaxStart - 1) + relaxed) / (variables.length - relaxStart)
      size += math.log(relaxed)
    }

    for(i <- (0 until relaxStart).map(x => varArray(x))){
      solver.add(variables(i) === currentSol.values(i)) //Freezing var => propagation occurs
      if (!variables(i).isBound) throw Inconsistency
      updateCloseness(i)
    }
//    println("relaxation done, " + relaxStart + " vars frozen")
  }

  override def toString: String = {
    var str = ""
    for(i <- 0 until nVars){
      for(j <- 0 until nVars) str += "(" + closeness(i)(j) + ", " + nImpacted(i)(j) + ")  "
      str += "\n"
    }
    str
  }
}

object PropagationGuidedRelax{
  var varSeq: Seq[CPIntVar] = Seq()
  lazy val propGuidedStore: PropagationGuidedRelax = new PropagationGuidedRelax(varSeq) // Closeness store used for propagation guided relax

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def propagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit = {
    if(varSeq.isEmpty) varSeq = vars.toSeq
    propGuidedStore.propagationGuidedRelax(solver, currentSol, s)
  }

  /**
   * Relaxes variables using propagation to guide the relaxation until the estimated size s of the neighbourhood is attained.
   * @param s The estimated size of the neighbourhood to attain.
   */
  def reversedPropagationGuidedRelax(solver: CPSolver, vars: Iterable[CPIntVar], currentSol: CPIntSol, s: Double): Unit = {
    if(varSeq.isEmpty) varSeq = vars.toSeq
    propGuidedStore.reversedPropagationGuidedRelax(solver, currentSol, s)
  }

}
