package oscar.cp.examples


import oscar.algo.Inconsistency
import oscar.algo.search.{DFSLinearizer, SearchStatistics}
import oscar.cp._
import oscar.cp.constraints.sequence.{First, Last, MaxDistance, MaxDistanceSOP, SeqDFSReplayer}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPInsertSeqVar, CPSeqVar}

import scala.collection.mutable.ArrayBuffer

object TSPwithAPC extends CPModel with App {
  for (tot <- List(6)) { //List(6,7,8,9,10)) { // easy instances
  //for (tot <- List(15,20,25,30,35,40)) { // medium instances
  //for (tot <- List(50,60,70,80,90,100)) { // hard instance
    for (perc <- List(40).map(i => tot * i / 100)) {//List(40, 50, 60, 70, 80).map(i => tot * i / 100)) {
      val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Easy/SPPWP_" + tot + "_" + perc + ".txt") // easy instances
      //val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Medium/SPPWP_" + tot + "_" + perc + ".txt") // medium instances
      //val data = SPPWPPaser.paser("Data/ShortestPathWithPreOrder/Hard/SPPWP_" + tot + "_" + perc + ".txt") // hard instances
      val name = "SPPWP_" + tot + "_" + perc
      val solveur = new SPPWPCPModel(data, 300)
      println(name + " | " + solveur.sol + " | " + solveur.starts.time + " | " + solveur.starts.nNodes + " | " + solveur.starts.nFails + " | " + solveur.starts.nSols
        + " || " + solveur.statsReplay.time + " | " + solveur.statsReplay.nNodes + " | " + solveur.statsReplay.nFails + " | " + solveur.statsReplay.nSols + " | ")
      //println(solveur.starts.completed + "         " +solveur.statsReplay.completed + "      "+ solveur.sol)
    }
  }
}

class SPPWPCPModel(val instance: SPPWPInstance,
                   val searchTime: Int = Integer.MAX_VALUE,
                   var maxDistParams: Boolean = false,
                   val solverSilent: Boolean = true,
                   ) extends CPModel {
  solver.silent = solverSilent
  val log: ArrayBuffer[String] = ArrayBuffer[String]()

  /*
   * Data
   */
  val n = instance.nNode
  var order: Seq[Int] = instance.order
  val transition: Array[Array[Int]] = Array.tabulate(n+1,n+1)((i,j) => 0)
  for(i <- 0 to n; j <- 0 to n if i < n && j < n)
    transition(i)(j) = instance.transition(i)(j)

  val startDepos = 0
  val endDepos = n

  /*
   * variables
   */
  val sequence = CPInsertSeqVar(n+1)

  val distance = CPIntVar(0 until 24000)//transition.flatten.sum)

  // Constraints
  // Setting depots:
  add(First(sequence, startDepos))
  add(Last(sequence, endDepos))

  // insert the nodes as of order after the startDepos as member of the sequence required
  sequence.markInsertedAfter(order, startDepos)

  // Mark the remaining nodes as required but not member of the sequence
  for(i <- 0 to n if !order.contains(i)) sequence.requires(i)

  //Distance constraints:
  if(maxDistParams)
    add(MaxDistance(sequence, distance, transition), CPPropagStrength.Weak)
  else
    add(MaxDistanceSOP(sequence, distance, transition), CPPropagStrength.Weak)
  // Objective
  minimize(distance)

  var sol: Double = Double.MaxValue

  onSolution {
    if (!solver.silent) {
      println("Total distance :" + distance.value.toDouble / 100)
      println(sequence.mkString(" - ") + "        " + order.mkString(" - "))
      println("--------------------------------------")
    }else
      sol = Math.min(sol, distance.value.toDouble / 100)
  }


  search{
    val unInsert = (0 until n).filterNot(i => sequence.isMember(i))
    if(unInsert.isEmpty) noAlternative
    else{
      //println(distance)
      val lessInsertion = unInsert.minBy(i => sequence.nCurrentInsertionsFor(i))
      val insert: Array[(Int,Int)]= sequence.allCurrentInsertionsFor(lessInsertion).map(pred => (pred,lessInsertion)).toArray
      if(insert.isEmpty) branchOne(throw Inconsistency)
      else branchAll(insert){
        case (pred, elem) => sequence.insertAfter(elem, pred)
      }
    }
  }





  val timeFactor = 2
  val linearizer: DFSLinearizer = new DFSLinearizer()
  val searchStart: Long = System.nanoTime()

  if (!solver.silent) println("starting initial search")
  val starts = startSubjectTo(searchListener = linearizer, timeLimit = searchTime){
    maxDistParams = true

  }
  if (!solver.silent) println(starts)
  log += starts.toString + "\n"

  if (!solver.silent) println("initial search done")

  // Replay with new params
  val statsReplay = replaySubjectTo(linearizer, Seq(sequence), timeLimit = searchTime*timeFactor) {
    maxDistParams = false
    solver.objective.objs.head.relax()
  }

  if (!solver.silent) println(statsReplay)
  log += statsReplay.toString + "\n"

  def replaySubjectTo(dfsLinearizer: DFSLinearizer, solutionVariables: Seq[CPSeqVar], timeLimit: Int = Int.MaxValue)(block: => Unit):
  SearchStatistics = {
    solver.pushState() // Store the current state
    block
    val stats = new SeqDFSReplayer(solver, solutionVariables).replay(dfsLinearizer.decisions, timeLimit)
    solver.pop()
    stats
  }
}














/*def selectionVar(): Array[(Int, Int)] = {
  val order = sequence.allMembers.drop(1).dropRight(1)
  val m = order.size
  val required = sequence.allRequired.toList.filter(r => r != sequence.allMembers(0) && r != sequence.allMembers.last)
  val sp = dynamicProgSOP(m+1, sequence, transition)
  var minSp = required.filter(r => !order.contains(r)).map(r => sp(required.indexOf(r))(m+1)(m)).min
  val select: Array[Int] = required.filter(r => sp(required.indexOf(r))(m+1)(m) == minSp).toArray
  val insertion : ArrayBuffer[(Int, Int)] = new ArrayBuffer[(Int,Int)]()
  for(i <- select){
    for(pred <- sequence.currentInsertionsPerElem(i))
      insertion += ((pred, i))
  }
  insertion.toArray
}*/

/*search{
  val unInsert = (0 until n).filterNot(i => sequence.isMember(i))
  if(unInsert.isEmpty) noAlternative
  else{
    val insert = selectionVar()
    if(insert.isEmpty) branchOne(throw Inconsistency)
    else branchAll(insert){
      case (pred, elem) => sequence.insertAfter(elem, pred)
    }
  }
}*/

/*search{
  val unInsert = (0 until n).filterNot(i => sequence.isMember(i))
  if(unInsert.isEmpty) noAlternative
  else{
    val lessInsertion = unInsert.minBy(i => unInsert.indexOf(i))
    val insert: Array[(Int,Int)]= sequence.allCurrentInsertionsFor(lessInsertion).map(pred => (pred,lessInsertion)).toArray
    if(insert.isEmpty) branchOne(throw Inconsistency)
    else branchAll(insert){
      case (pred, elem) => sequence.insertAfter(elem, pred)
    }
  }
}*/

/*var solut = Double.MaxValue

def dynamicProgSOP(p: Int, sequence: CPInsertSeqVar, transition: Array[Array[Int]]): Array[Array[Array[Int]]] ={
  val first = sequence.allMembers(0)
  val last = sequence.allMembers.last
  val required = sequence.allRequired.toList.filter(r => r != first && r != last)
  val n = required.length
  val order = sequence.allMembers.drop(1).dropRight(1)
  val m = order.length
  val shortPath: Array[Array[Array[Int]]] = Array.ofDim[Int](n,p+1,m+1)
  for(e <- 0 to p){
    for(s <- 0 to m){
      for(i <- 0 until n) {
        shortPath(i)(e)(s) = Int.MaxValue
        if(s == 0 && e == 0 && (required(i) == first || required(i) == last))
          shortPath(i)(e)(s) = 0
        if(s > 0){
          if(e == s && required(i) == order(s-1))
            shortPath(i)(e)(s) = pathLength(first, order.dropRight(m-s), transition)
          if(e > s && required(i) != first){
            val orderMinus = order.dropRight(m-s)
            if(orderMinus.contains(required(i))){
              if(required(i) == orderMinus.last){
                val orderM = orderMinus.dropRight(1)
                for (a <- required if !order.contains(a) || ( orderM.nonEmpty && orderM.contains(a) && a == orderM.last)) {
                  if (a != first && a != required(i) && shortPath(required.indexOf(a))(e - 1)(s - 1) != Int.MaxValue) {
                    shortPath(i)(e)(s) = Math.min(shortPath(i)(e)(s), shortPath(required.indexOf(a))(e - 1)(s - 1) + transition(a)(required(i)))
                  }
                }
              }
            }else{
              for (a <- required if !order.contains(a) || (orderMinus.nonEmpty && orderMinus.contains(a) && a == orderMinus.last)) {
                if (a != first && a != required(i) && shortPath(required.indexOf(a))(e - 1)(s) != Int.MaxValue) {
                  shortPath(i)(e)(s) = Math.min(shortPath(i)(e)(s), shortPath(required.indexOf(a))(e - 1)(s) + transition(a)(required(i)))
                }
              }
            }
          }
        }
      }
    }
  }
  //shortPath(required.indexOf(u))(p)(m)
  shortPath
}
def pathLength(first: Int, perm: Seq[Int], transition: Array[Array[Int]]): Int ={
  var score: Int = 0
  for(i <- 0 until perm.length){
    if(i == 0)
      score += transition(first)(perm(0))
    else
      score += transition(perm(i-1))(perm(i))
  }
  score
}*/

