package oscar.examples.cp

import oscar.cp._
import oscar.cp.constraints.scheduling.LinearTT
import oscar.cp.scheduling.constraints.TTPerTask

object RCPSP extends CPModel with App {
  
  // (duration, consumption)
  val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2), (20, 2), (80, 1))
  //val instance = Array((5, 1), (3, 1), (9, 3), (1, 2), (2, 2), (8, 1), (3, 2), (2, 2), (2, 1), (1, 1), (1, 2), (2, 2), (8, 1))
  val durationsData = instance.map(_._1)
  val demandsData = instance.map(_._2)
  val capa = 4
  val horizon = durationsData.sum
  val nTasks = durationsData.length

  solver.silent = true

  val durations = Array.tabulate(nTasks)(t => CPIntervalVar(durationsData(t)))
  val starts = Array.tabulate(nTasks)(t => CPIntervalVar(0, horizon - durations(t).min))
  val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
  val demands = Array.tabulate(nTasks)(t => CPIntervalVar(demandsData(t)))
  val makespan = maximum(ends)
  
  val mirStarts = ends.map(-_)
  val mirEnds = starts.map(-_)
  
  add(new LinearTT(starts, durationsData, ends, demandsData, capa))
  add(new LinearTT(mirStarts, durationsData, mirEnds, demandsData, capa))
  //add(new TTPerTask(starts, durations, ends, demands, Array.fill(nTasks)(CPIntervalVar(1)), CPIntervalVar(capa), 1))
  
  //add(maxCumulativeResource(starts, durations, ends, demands, CPIntervalVar(capa)),Weak)
  
  minimize(makespan) 
  
  search {
    /*val sorted = starts//.sortBy(_.min)
    val x = sorted.find(!_.isBound)
    if (!x.isDefined) noAlternative
    else {
      val variable = x.get
      val v = variable.min
      branch(post(variable == v))(post(variable > v))
    }*/
    setTimes(starts, durations, ends)
  }
  
  onSolution { 
    println(makespan.value)
  }
    
  val stats = start()
  
  println(stats)
}