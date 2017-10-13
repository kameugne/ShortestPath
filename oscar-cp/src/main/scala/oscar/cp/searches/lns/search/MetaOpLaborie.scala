package oscar.cp.searches.lns.search

import oscar.algo.Inconsistency
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.operators.{ALNSOperator, ALNSReifiedOperator}
import oscar.cp.searches.lns.selection.{AdaptiveStore, Metrics, RouletteWheel}
import oscar.cp.{CPIntVar, CPSolver}

import scala.util.Random

/**
  * TODO
  */
class MetaOpLaborie(solver: CPSolver, vars: Array[CPIntVar], config: ALNSConfig) extends ALNSSearchImpl(solver, vars, config) {
//  val tolerance: Double = config.metaParameters.getOrElse('tolerance, 0.25).asInstanceOf[Double]
  override val stagnationThreshold = 10

  override lazy val relaxOps: Array[ALNSOperator] = config.relaxStore.getElements.toArray
  override lazy val searchOps: Array[ALNSOperator] = config.searchStore.getElements.toArray

  override lazy val relaxStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    relaxOps,
    0.05,
    false,
    checkEfficiency
  )

  override lazy val searchStore: AdaptiveStore[ALNSOperator] = new RouletteWheel[ALNSOperator](
    searchOps,
    0.05,
    false,
    checkEfficiency
  )

  override def alnsLoop(): Unit = {
    if (!solver.silent) println("\nStarting adaptive LNS...")
    stagnation = 0

    timeLearning()

    while (
      System.nanoTime() < endTime &&
        relaxStore.nonActiveEmpty &&
        searchStore.nonActiveEmpty &&
        !optimumFound
    ) {
      val relax = relaxStore.select()
      val search = searchStore.select()
      /*if(stagnation >= stagnationThreshold && previousBest.isDefined) {
        lnsIter(relax, search, previousBest.get)
      }
      else*/ lnsIter(relax, search)
    }
  }

  protected def timeLearning(): Unit = {
    learning = true
    iterTimeout = config.timeout
    Random.shuffle(relaxOps.toSeq).foreach(relax => {
      val search = searchOps(Random.nextInt(searchOps.length))
      lnsIter(relax, search)
    })
    Random.shuffle(searchOps.toSeq).foreach(search => {
      while(search.execs < 1) {
        val relax = relaxStore.select()
        lnsIter(relax, search)
      }
    })
    learning = false

    maxOpIterTimeout()
    if(!solver.silent) println("learning done, iterTimeout: " + iterTimeout)
  }

  protected def checkEfficiency(op: ALNSOperator): Double = {
    if(op.name != "dummy"){
      val opEfficiency = Metrics.efficiencyFor(op, iterTimeout)
      val searchEfficiency = Metrics.searchEfficiencySince(solsFound, Math.min(timeInSearch - iterTimeout * 5, if (solsFound.nonEmpty) solsFound.last.time else 0L), timeInSearch)

      if (!solver.silent) {
        println("Search efficiency is " + searchEfficiency)
        println("Operator " + op.name + " efficiency is " + opEfficiency)
      }

      if (op.time >= iterTimeout * 2 && /*(opEfficiency < searchEfficiency * tolerance ||*/ op.sols == 0/*)*/){
        op.setActive(false)
        if (!solver.silent) println("Operator " + op.name + " deactivated due to low efficiency!")
//        manageIterTimeout()
      }

      opEfficiency
    }
    else 1.0
  }

  protected def maxOpIterTimeout(): Unit = {
    var maxTime = 0L
    searchOps.filter(op => {
      op.isActive && op.execs > 0 && op.sols > 0
    }).foreach(op => {
      val avgTime = Metrics.avgTime(op)
      if (avgTime > maxTime) maxTime = avgTime.ceil.toLong
    })
    iterTimeout = if(maxTime == 0L || learning) config.timeout else maxTime
  }

  override protected def lnsIter(relax: ALNSOperator, search: ALNSOperator, sol: CPIntSol = currentSol.get): Unit = {
    if(!learning) endIter = Math.min(System.nanoTime() + iterTimeout, endTime)

    if(!solver.silent){
      println("\nStarting new search with: " + relax.name + " and " + search.name)
      println("Operator timeout: " + (endIter - System.nanoTime())/1000000000.0 + "s")
    }

    //New search using selected strategies:
    val (relaxFunction, _, _) = relax.getFunction
    val (searchFunction, searchFailures, searchDiscrepancy) = search.getFunction
    if(searchFailures.isDefined) nFailures = searchFailures.get

    val searchObjective = currentSol.get.objective
    if(sol.objective != searchObjective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = sol.objective
      currentSol = Some(sol)
    }

    do {
      var relaxDone = true
      val startObjective = currentSol.get.objective

      val iterStart = System.nanoTime()

      val stats = solver.startSubjectTo(stopCondition, searchDiscrepancy.getOrElse(Int.MaxValue), null) {
        try {
          relaxFunction(currentSol.get)
          searchFunction(currentSol.get)
        }
        catch {
          case _: Inconsistency => relaxDone = false
        }
      }

      val iterEnd = System.nanoTime()
      val newObjective = currentSol.get.objective

      if(searchFailures.isDefined) nFailures = 0 //Resetting failures number to 0

      val improvement = math.abs(newObjective - startObjective)
      val time = iterEnd - iterStart

      if(math.abs(newObjective - searchObjective) > 0) stagnation = 0
      else stagnation += 1

      if (!solver.silent){
        if(!relaxDone) println("Search space empty, search not applied, improvement: " + improvement)
        else if(stats.completed) println("Search space completely explored, improvement: " + improvement)
        else println("Search done, Improvement: " + improvement)
      }

      //Updating probability distributions:
      relax.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = !relaxDone && !learning, iter)


      if(relaxDone || relax.name == "dummy"){
        search.update(iterStart, iterEnd, startObjective, newObjective, stats, fail = false, iter)
      }
    }while(System.nanoTime() < endIter && !learning)

    if(!relax.isInstanceOf[ALNSReifiedOperator]){
      val relaxScore = if (relax.isActive) relaxStore.adapt(relax)
      else {
        if (!solver.silent) println("Operator " + relax.name + " deactivated")
        if (!learning) relaxStore.deactivate(relax)
        -1.0
      }
      history += ((timeInSearch, relax.name, relaxScore))
    }

    if(!search.isInstanceOf[ALNSReifiedOperator]){
      val searchScore = if (search.isActive) searchStore.adapt(search)
      else {
        if (!solver.silent) println("Operator " + search.name + " deactivated")
        if (!learning) searchStore.deactivate(search)
        -1.0
      }
      history += ((timeInSearch, search.name, searchScore))
    }

    if(currentSol.get.objective != bestSol.get.objective){
      solver.objective.objs.head.relax()
      solver.objective.objs.head.best = bestSol.get.objective
      currentSol = bestSol
    }
    iter += 1
  }
}
