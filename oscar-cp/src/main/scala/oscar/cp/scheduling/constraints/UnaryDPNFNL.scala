package oscar.cp.scheduling.constraints

import oscar.cp.core._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPOutcome._
import oscar.algo.SortUtils._
import scala.math.{min, max}
import scala.annotation.tailrec
import oscar.cp.scheduling.util.OpenSparseSet

/**
 * @author Steven Gay steven.gay@uclouvain.be
 */
/*
 *  Unary Detectable Precedences, with optional tasks and non-constant durations.
 *  As in "Extension of O(n log n) Filtering...", Vilim et al. Constraints 2005. 
 *  Added NFNL, since it fill the theta tree in the same order.
 */

class UnaryDPNFNL(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore)
extends Constraint(store, "UnaryDetectablePrecedences") {
  val lr = new UnaryDPNFNLLR(starts, durations, ends, resources, id) 
  val rl = new UnaryDPNFNLLR(ends map(-_), durations, starts map(-_), resources, id)
  
  override def setup(strength: CPPropagStrength) = {
    try {
      if (store.add(Array(lr, rl)) == Failure) Failure
//      if (store.add(Array(lr)) == Failure) Failure
      else Suspend
    }
    catch {
      case e: NoSolutionException => Failure
      case e: Inconsistency => Failure
    }
  }
}




class UnaryDPNFNLLR(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore)
//extends Constraint(store, "UnaryDPNFNLLR")
extends UnaryTemplate(starts, durations, ends, resources, id, "UnaryDetectablePrecedencesLR")(store)
{
  private[this] val nTasks = starts.length

  // private final val compareFlag = true
  priorityL2 = 3
  
  private def nextPowerOfTwo(k: Int): Int = {
    1 << math.ceil(math.log(k) / math.log(2)).toInt
  }
  
  private[this] val workload    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks)) 
  private[this] val envelope    = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // base envelope level
  private[this] val workloadOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum workload of this node using at most one optional
  private[this] val envelopeOpt = Array.ofDim[Int](2 * nextPowerOfTwo(nTasks))   // maximum envelope of an optional activity at this node

  private[this] val leafOfActivity  = Array.ofDim[Int](nTasks)  // activity -> leaf id
  
  // for mergeSort
  private[this] val temp1 = Array.ofDim[Int](nTasks + 1)
  private[this] val temp2 = Array.ofDim[Int](nTasks + 1)

  private[this] val sortedBySMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMax = Array.tabulate(nTasks){ i => i }
  private[this] val sortedByEMin = Array.tabulate(nTasks){ i => i }
  private[this] val sortedBySMax = Array.tabulate(nTasks){ i => i }
  
  private[this] val toConsiderBySMin = Array.ofDim[Int](nTasks)
  private[this] val toConsiderByEMax = Array.ofDim[Int](nTasks)
  private[this] val toConsiderByEMin = Array.ofDim[Int](nTasks)
  private[this] val toConsiderBySMax = Array.ofDim[Int](nTasks)
  
  
  override def propagate(): CPOutcome = {
    updateCache()
    
    // removeExtremal()
    
    // Step 1: Initialization
    // Step 1.1: sort activities by criteria of interest
    // TODO: we will only introduce task with smax < max{emin}, filter out the others
    // TODO: with variable durations, we may be more precise by adding the energy of i, dmin(i),
    //       not at smin(i), but at emin(i) - dmin(i)
    // TODO: we can skip the introduction of tasks with dmin(i) == 0 in the tree, however pruning these tasks is crucial
    
    filterSort(sortedBySMin, toConsiderBySMin, smin)
    filterSort(sortedByEMax, toConsiderByEMax, emax)
    filterSort(sortedByEMin, toConsiderByEMin, emin)
    filterSort(sortedBySMax, toConsiderBySMax, smax)

    // Step 1.2: initialize arrays of the tree
    /*
     *  Tree uses indices from 0 to 2 * nodes - 1
     *  Leaves go from nodes to 2 * nodes - 1
     *  Leaves are not all actually used, only those from nodes to nodes + nToConsider - 1  
     */
    val nToConsider = toConsider.limit.value
    val nodes = nextPowerOfTwo(nToConsider)
    
    // if (debug) println(s"UnaryDPNFNL: considering $nToConsider over $nTasks tasks, making a tree of ${2*nodes} nodes.")
    // if (nodes < nToConsider) throw new IllegalArgumentException()
    
    // make a map from activities to leaves
    var p = nToConsider
    while (p > 0) {
      p -= 1
      val a = toConsiderBySMin(p)
      leafOfActivity(a) = nodes + p
    }

    // initialize nodes
    p = 2 * nodes
    while (p > 0) {
      p -= 1
      workload(p) = 0
      envelope(p) = Int.MinValue
      workloadOpt(p) = 0
      envelopeOpt(p) = Int.MinValue      
    }

    // Step 2: Do main loop
    var pEMin = 0
    var pEMax = 0
    var count = 2 * nToConsider
    
    var q = 0
    var jj = 0
    
    while (count > 0) {
      count -= 1
      
      // find next event, either emin or emax
      // the last pruning event is always an emax event, we can always read those values.
      var i         = toConsiderByEMax(pEMax)
      var eventDate = emax(i)
      var emaxEvent = true
      
      if (pEMin < nToConsider && emin(toConsiderByEMin(pEMin)) <= eventDate) {
        i         = toConsiderByEMin(pEMin)
        eventDate = emin(i)
        emaxEvent = false
        pEMin += 1
      }
      else pEMax += 1
      
      // insert tasks that must be before pruning event
      while (q < nToConsider && smax(toConsiderBySMax(q)) < eventDate) {
        val j = toConsiderBySMax(q)
        
        if (required(j)) {
          jj = j
          addToTheta(j)
        }
        else addToLambda(j)

        q += 1
      }
      
      
      if (!required(i)) {
        if (envelope(1) > smax(i)) {
          if ((emaxEvent && smax(jj) < emin(i)) || !emaxEvent) {
            if (resources(i).removeValue(id) == Failure) throw Inconsistency
            // println((if (emaxEvent) "NFNL" else "DP") + " removing with a push")
            toConsider.exclude(i)
            removeFromLambda(i)
          }
        }
      }
      else { //} && dmin(i) > 0) {    // remove impossible optionals
        val mustRemoveI = workload(leafOfActivity(i)) > 0
        if (mustRemoveI) removeFromTheta(i)
        
        if (emaxEvent) {
          if (envelope(1) > smax(i)) {
            val smaxJ = smax(jj)
            if (smaxJ < emax(i)) {
              // if (compareFlag) println("NFNL pushing")
              if (ends(i).updateMax(smaxJ) == Failure) throw Inconsistency
            }
          } 
        }
        else {
          if (envelope(1) > smin(i)) {
            // if (compareFlag) println("DP pushing")
            if (starts(i).updateMin(envelope(1)) == Failure) throw Inconsistency
            
          }
        }
        
        if (!emaxEvent) {
          while (envelopeOpt(1) > smax(i)) {
            val opt = getMaxOptional(1, nodes)  // get responsible optional leaf
            val b = toConsiderBySMin(opt-nodes)  // get activity of that leaf
            
            if (resources(b).removeValue(id) == Failure) throw Inconsistency
            toConsider.exclude(b)
            removeFromTheta(b)
          }
        }
        
        if (mustRemoveI) addToTheta(i)
        
      }
      
    }
    
    removeExtremal()
    // removeIsolated(toConsiderBySMin, toConsiderByEMax, nToConsider)
    Suspend
  }
  
  @inline final def addToTheta(act: Int) = {
    val leaf = leafOfActivity(act)
    workload(leaf)    = dmin(act)
    envelope(leaf)    = smin(act) + dmin(act)
    workloadOpt(leaf) = dmin(act)
    envelopeOpt(leaf) = smin(act) + dmin(act)
    
    // if (debug) println(s"envelope ${envelope(leaf)}")
    
    insertActivity(leaf / 2)
  }

  @inline final def addToLambda(act: Int) = {
    val leaf = leafOfActivity(act)
    // workload(leaf)    = 0
    // envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = dmin(act)
    envelopeOpt(leaf) = smin(act) + dmin(act)
    
    insertActivity(leaf / 2)
  }
  
  @inline final def removeFromTheta(act: Int) = {
    val leaf = leafOfActivity(act)
    workload(leaf)    = 0
    envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = 0
    envelopeOpt(leaf) = Int.MinValue
    
    insertActivity(leaf / 2)
  }

  @inline final def removeFromLambda(act: Int) = {
    val leaf = leafOfActivity(act)
    // workload(leaf)    = 0
    // envelope(leaf)    = Int.MinValue
    workloadOpt(leaf) = 0
    envelopeOpt(leaf) = Int.MinValue
    
    insertActivity(leaf / 2)
  }
  

  @tailrec
  final def insertActivity(t: Int): Unit = {
    if (t > 0) {
      val left = t << 1
      val right = 1 + left
        
      workload(t) = workload(left) + workload(right)
      envelope(t) = max(envelope(left) + workload(right), envelope(right))
      
      workloadOpt(t) = max(workloadOpt(left) + workload(right),
                           workload(left) + workloadOpt(right))
                           
      envelopeOpt(t) = max(envelopeOpt(right),
                         max(envelope(left) + workloadOpt(right),
                             envelopeOpt(left) + workload(right))
                        )
      
      // if (debug) println(s"envelope ${envelope(t)}, left = ${envelope(left)}, workload = ${workload(right)}, right = ${envelope(right)}")
      insertActivity(t/2)
    } 
  }
  
  /*
   * find which optional activity causes the value of envelopeOpt
   * This method is more lengthy than remembering the activity at each node,
   * but when activity removal is rare, recomputing on need is cheaper.
   */
  @tailrec
  final def getMaxOptional(t:Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val e = envelopeOpt(t) 
      
      if (e == envelopeOpt(right)) {
        getMaxOptional(right, tMax)
      }
      else if (e == envelopeOpt(left) + workload(right)) {
        getMaxOptional(left, tMax)
      }
      else {
        getMaxOptionalWorkload(right, tMax)
      }
    }
  }
  
  
  /*
   * find which optional activity causes the value of workloadOpt
   */
  @tailrec
  final def getMaxOptionalWorkload(t: Int, tMax: Int): Int = {
    if (t >= tMax) t  // reached a leaf
    else {
      val left = t << 1
      val right = 1 + left
      val w = workloadOpt(t)
      
      if (w == workloadOpt(left) + workload(right)) {
        getMaxOptionalWorkload(left, tMax)
      }
      else {
        assert(w == workload(left) + workloadOpt(right))
        getMaxOptionalWorkload(right, tMax)
      }
        
    }
  }
  
  private[this] val indices = new Array[Int](nTasks)
  // filters out tasks that should not be considered, then sorts them
  final def filterSort(byKey: Array[Int], filtered: Array[Int], keys: Array[Int]): Unit = {
    val limit = toConsider.limit.value
    val status = toConsider.status
    
    var p = byKey.length 
    var q = limit
    
    // extract only values to consider
    while (p > 0) {
      p -= 1
      val task = byKey(p)
      if (status(task) < limit) {
        q -= 1
        filtered(q) = task
        indices(q) = p
      }
    }
    
    // sort them
    mergeSort(filtered, keys, 0, limit, temp1, temp2)
    
    // put them back for mergeSort's incremental behaviour
    p = limit
    while (p > 0) {
      p -= 1
      byKey(indices(p)) = filtered(p) 
    }
  }

}

object UnaryDPNFNL {
  def apply(starts: Array[CPIntVar], durations: Array[CPIntVar], ends: Array[CPIntVar], resources: Array[CPIntVar], id: Int)(implicit store: CPStore) = 
    new UnaryDPNFNL(starts, durations, ends, resources, id) 

}