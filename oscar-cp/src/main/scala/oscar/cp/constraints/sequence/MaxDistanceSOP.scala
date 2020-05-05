package oscar.cp.constraints.sequence

import oscar.algo.graph.GraphUtils
import oscar.cp.core.variables.{CPIntVar,  CPInsertSeqVar}

class MaxDistanceSOP(sequence: CPInsertSeqVar, distance: CPIntVar, transitions: Array[Array[Int]])
  extends MaxDistance(sequence, distance, transitions) {
  private def evaluateSeqPath(seq: Seq[Int]): (Seq[(Int, Int)], Int) = {
    if(seq.isEmpty) return (Seq(), 0)

    val edges = seq.zip(seq.drop(1))
    val dist = edges.map(edge => transitions(edge._1)(edge._2)).sum
    (edges, dist)
  }

  override def propagate(): Unit = {
    val seq = sequence.allMembers
    val (seqPath, seqDist) = evaluateSeqPath(seq)

    val first = sequence.allMembers(0)
    val last = sequence.allMembers.last
    val order = sequence.allMembers.drop(1).dropRight(1)
    val required = sequence.allRequired.toSeq.filter(r => r != first && r != last)
    val m = order.size
    val n = required.length
    val shortestPath: Array[Array[Array[Int]]] = dynamicProgSOP(n, order, required, transitions)
    val requiredLb = required.map(r => shortestPath(required.indexOf(r))(n)(m)).min
    currentDist.setValue(seqDist)
    distance.updateMin(seqDist)

    if(sequence.isBound){
      distance.updateMax(currentDist.value)
      deactivate()
    } else {
      for((elem, preds) <- sequence.currentInsertionsPerElem){
        for(pred <- preds){
          val next = sequence.nextMember(pred)

          //Computing distance cost of inserting element at given position:
          val diff = if(sequence.isEmpty) 0
          else if(pred == -1) transitions(elem)(next)
          else if(next == -1) transitions(pred)(elem)
          else transitions(pred)(elem) + transitions(elem)(next) - transitions(pred)(next)

          //Removing insertion if violation:
          val lastO = order.last
          if(currentDist.value + diff > distance.max)
            sequence.removeInsertion(elem, pred)
        }
      }

      //Update the min value of distance to the minimum distance of mandatory activities:
      if(sequence.requiredSize > sequence.length){
        distance.updateMin(requiredLb)
        //Excluding possibles activities if dist too long:
        val allPossibleNotRequired = sequence.allPossibleOrMember.filterNot(i => sequence.isRequired(i))
        for (actId <- allPossibleNotRequired) {
          var minDiff = Int.MaxValue
          //Computing the minimum distance cost of inserting element at any position:
          for(pred <- sequence.allCurrentInsertionsFor(actId)) {
            val next = sequence.nextMember(pred)
            val diff = if (sequence.isEmpty) 0
            else if (pred == -1) transitions(actId)(next)
            else if (next == -1) transitions(pred)(actId)
            else transitions(pred)(actId) + transitions(actId)(next) - transitions(pred)(next)
            minDiff = Math.min(minDiff, diff)
          }
          if (sequence.nonEmpty && requiredLb + minDiff > distance.max) {
            sequence.excludes(actId)
          }
          /* Alternative way to excluded possible activites if distance is too long: too cost pruning excluded possible activities */
          val sDepos = sequence.allMembers.head
          val eDepos = sequence.allMembers.last
          val orderAct = sequence.allMembers.drop(1).dropRight(1)
          val requiredAct = sequence.allRequired.toSeq.filter(r => r != sDepos && r != eDepos) ++ Seq(actId)
          val m = orderAct.size
          val n = requiredAct.length
          val shortestPathAct: Array[Array[Array[Int]]] = dynamicProgSOP(n, orderAct, requiredAct, transitions)
          val requiredLbAct = required.map(r => shortestPathAct(requiredAct.indexOf(r))(n)(m)).min
          if(requiredLbAct > distance.max){
            sequence.excludes(actId)
          }
        }
      }
    }
  }

  def dynamicProgSOP(p: Int, order: Seq[Int], required: Seq[Int], transition: Array[Array[Int]]): Array[Array[Array[Int]]] ={
    val first = sequence.allMembers(0)
    val last = sequence.allMembers.last
    val n = required.length
    val m = order.length
    val shortPath: Array[Array[Array[Int]]] = Array.ofDim[Int](n,p+1,m+1)
    for(e <- 0 to p){
      for(s <- 0 to m){
        for(i <- 0 until n) {
          shortPath(i)(e)(s) = Int.MaxValue
          if(s == 0 && e == 0)
            shortPath(i)(e)(s) = 0
          if(s > 0){
            if(e == s && required(i) == order(s-1))
              shortPath(i)(e)(s) = pathLength(first, order.dropRight(m-s), transition)
            if(e > s){
              val orderMinus = order.dropRight(m-s)
              if(orderMinus.contains(required(i))){
                if(required(i) == orderMinus.last){
                  val orderM = orderMinus.dropRight(1)
                  for (a <- required if !order.contains(a) || ( orderM.nonEmpty && orderM.contains(a) && a == orderM.last)) {
                    if (a != required(i) && shortPath(required.indexOf(a))(e - 1)(s - 1) != Int.MaxValue) {
                      shortPath(i)(e)(s) = Math.min(shortPath(i)(e)(s), shortPath(required.indexOf(a))(e - 1)(s - 1) + transition(a)(required(i)))
                    }
                  }
                }
              }else{
                for (a <- required if !order.contains(a) || (orderMinus.nonEmpty && orderMinus.contains(a) && a == orderMinus.last)) {
                  if (a != required(i) && shortPath(required.indexOf(a))(e - 1)(s) != Int.MaxValue) {
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
  }

}

object MaxDistanceSOP {
  def apply(sequence: CPInsertSeqVar, distance: CPIntVar, transitions: Array[Array[Int]]): MaxDistanceSOP =
    new MaxDistanceSOP(sequence, distance, transitions)
}
