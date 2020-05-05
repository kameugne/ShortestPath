package oscar.cp.examples

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.AlternativeActivities
import oscar.cp.constraints.sequence.{Cumulative, First, Last, MaxDistance, MaxDistanceSOP, Precedence, SequenceAllocation, TransitionTimes}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.{CPInsertSeqVar, CPSeqVar}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

object DARPWithInsertSeqVar extends CPModel with App {

  val instancePath = "Data/DARP/Cordeau2003/a3-24.txt"
  val scaling = 100
  val lines = Source.fromFile(instancePath).getLines()
  val header = lines.next().trim.split("\\s+")
  assert(header.length == 5)

  val nVehicle = header(0).toInt
  //  val nRequests = header(1).toInt
  val maxRouteDuration = (header(2).toDouble * scaling).round.toInt
  val vCapacity = header(3).toInt
  val maxRideTime = (header(4).toDouble * scaling).round.toInt

  def lineToStop(line: String): Stop = {
    val splitted = line.trim.split("\\s+")
    assert(splitted.length == 7)
    Stop(splitted(0).toInt, splitted(1).toDouble, splitted(2).toDouble, (splitted(3).toDouble * scaling).round.toInt, splitted(4).toInt, (splitted(5).toDouble * scaling).round.toInt, (splitted(6).toDouble * scaling).round.toInt)
  }
  val depot = lineToStop(lines.next())

  val stopLines = lines.filterNot(_.matches("\\s+")).toArray

  // number of trips
  val nRequests = stopLines.length / 2
  val stops = stopLines.map(lineToStop)

  def dist(i: Int, j: Int): Int = {
    val x = (sites(i).x - sites(j).x) * (sites(i).x - sites(j).x)
    val y = (sites(i).y - sites(j).y) * (sites(i).y - sites(j).y)
    (math.sqrt(x + y) * scaling).round.toInt
  }

  val nStop = stops.length

  // Generating stop for start end depot per vehicle
  val depots = Array.fill(2*nVehicle)(depot)

  val sites: Array[Stop] = stops ++ depots
  val nSite: Int = sites.length

  val distances = Array.tabulate(nSite, nSite)((i, j) => {
    dist(i, j)
  })
  val requests = Array.tabulate(nRequests)(r => DARPRequest(r, nRequests+r, maxRideTime))


  /* Variables */

  val arrival: Array[CPIntVar] = Array.tabulate(nSite)(i => {
    CPIntVar(sites(i).winStart to sites(i).winEnd)
  })

  val duration: Array[CPIntVar] = Array.tabulate(nSite)(i => {
    CPIntVar(sites(i).service)
  })

  val departure: Array[CPIntVar] = Array.tabulate(nSite)(i => arrival(i) + duration(i))

  val siteVehicle: Array[CPIntVar] = Array.tabulate(nSite)(i => {
    if (sites(i).isDepot) CPIntVar((i - nStop) % nVehicle)
    else CPIntVar(0 until nVehicle)
  })

  val travelLoad: Array[CPIntVar] = Array.tabulate(nRequests)(i => CPIntVar(sites(i).load))

  val optionalArrival: Array[Array[CPIntVar]] = Array.tabulate(nVehicle + 1, nSite)((_, s) => CPIntVar(arrival(s).min, arrival(s).max))
  val optionalDuration: Array[Array[CPIntVar]] = Array.tabulate(nVehicle + 1, nSite)((_, s) => CPIntVar(duration(s).min, duration(s).max))
  val optionalDeparture: Array[Array[CPIntVar]] = Array.tabulate(nVehicle + 1, nSite)((v, s) => optionalArrival(v)(s) + optionalDuration(v)(s))

  //Sequence variables:
  val sequences: Array[CPInsertSeqVar] = Array.fill(nVehicle)(CPInsertSeqVar(sites.length))

  val served: Array[CPBoolVar] = Array.fill(nRequests)(CPBoolVar(b = true)) //All requests are mandatory
  val routeCost: Array[CPIntVar] = Array.tabulate(nVehicle)(v => CPIntVar(0, maxRouteDuration))

  /*  constraints */

  //Setting depots:
  for (v <- sequences.indices) {
    add(First(sequences(v), nStop + v))
    add(Last(sequences(v), nStop + nVehicle + v))
  }

  //Sequence Allocation
  add(SequenceAllocation(sequences.asInstanceOf[Array[CPSeqVar]], sites.indices, siteVehicle))


  //Linking optional and real time windows:
  for (s <- sites.indices) {
    add(AlternativeActivities(
      arrival(s),
      duration(s),
      departure(s),
      optionalArrival.map(_ (s)),
      optionalDuration.map(_ (s)),
      optionalDeparture.map(_ (s)),
      siteVehicle(s)
    ))
  }

  //Precedence constraints:
  val precConstraints: Array[Seq[Precedence]] = requests.map(r => {
    sequences.indices.map(v => Precedence(sequences(v), r.pickup, r.drop, dependent = true))
  })

  //Transition time constraints:
  val ttConstraints: Seq[TransitionTimes] = sequences.indices.map(v =>
    TransitionTimes(sequences(v), optionalArrival(v), optionalDuration(v), optionalDeparture(v), distances)
  )


  //Distance constraints:
  val distConstraints: Seq[MaxDistance] = sequences.indices.map(v =>
    MaxDistanceSOP(sequences(v), routeCost(v), distances)
  )

  //Cumul constraints:
  val (startSites, endSites) = requests.map{t => (t.pickup, t.drop)}.unzip
  val cumulConstraints: Seq[Cumulative] = sequences.indices.map(v => {
    val maxCapVar = CPIntVar(vCapacity)
    val minCapVar = CPIntVar(0)
    Cumulative(sequences(v), startSites, endSites, travelLoad, maxCapVar, minCapVar)
  })

  //Adding DARP constraints:
  for (v <- 0 until nVehicle) {
    add(ttConstraints(v), CPPropagStrength.Weak)
    add(distConstraints(v), CPPropagStrength.Weak)
    add(cumulConstraints(v))
    for(r <- requests.indices) add(precConstraints(r)(v))
  }

  // ride time constraints
  for (r <- requests.indices) add(arrival(requests(r).drop) - departure(requests(r).pickup) <= maxRideTime)

  // additional distance constraint
  for (r <- requests.indices) add(arrival(requests(r).drop) - departure(requests(r).pickup) >= distances(requests(r).pickup)(requests(r).drop))

  for(r <- requests.indices)
    add(siteVehicle(r) === siteVehicle(r+nRequests))

  /* Objective */

  val totalRouteCost: CPIntVar = sum(routeCost)
  minimize(totalRouteCost)



  /* Solution */

  var currentSol: (Seq[Seq[Int]], Array[Int]) = (Seq(), Array())

  onSolution {
    println("Total Route Cost: " + totalRouteCost.value)
    println("Sequences:\n" + sequences.mkString("\n"))

    currentSol = (sequences.map(_.allMembers), siteVehicle.map(_.value))

    println("------------")
  }

  /*def relaxSelection(d: Double): Set[Int] ={
    var remainingRequests = (0 until nRequests).toArray
    val relaxedRequest = ArrayBuffer[Int]()
    var numberRequestsToRelax = (d * nRequests).toInt
    val rand =  new Random()
    while(numberRequestsToRelax > 0){
      val index = rand.nextInt(remainingRequests.length)
      if(index != remainingRequests.length-1){
        var temp = remainingRequests(index)
        remainingRequests(index) = remainingRequests(remainingRequests.length-1)
        remainingRequests(remainingRequests.length-1) = temp
      }
      relaxedRequest +=  remainingRequests(index)
      remainingRequests = remainingRequests.dropRight(1)
      numberRequestsToRelax -= 1
    }
    relaxedRequest.toSet
  }

  def requestRelax(relaxSize: Double): Unit = {
    //    println("Starting relax")
    //Temporarily deactivating darp constraints:
    for (v <- sequences.indices){
      ttConstraints(v).deactivate()
      cumulConstraints(v).deactivate()
      distConstraints(v).deactivate()
      for(r <- requests.indices) precConstraints(r)(v).deactivate()
    }

    val relaxedRequests = relaxSelection(relaxSize).union(relaxSelection(relaxSize).map(i => i+nRequests)) //Selecting patients to relax
    val noRelaxedSites = siteVehicle.indices.toSet.diff(relaxedRequests)

    //Setting up vehicles:
    for(i <- noRelaxedSites)  add(siteVehicle(i) === currentSol._2(i))
    //for(i <- siteVehicle.indices) if(!relaxedRequests.contains(sites(i).request)) add(siteVehicle(i) === currentSol._2(i))

    //Setting up sequences:
    for (v <- sequences.indices) {
      val sitesToFix = currentSol._1(v).filter(i => noRelaxedSites.contains(i) && !sites(i).isDepot)
      if (sitesToFix.nonEmpty) sequences(v).markInsertedAfter(sitesToFix, nStop + v)
    }

    //Setting up rem requests:
    //for(req <- (0 until nRequests).filterNot(relaxedRequests.contains)) remReqs.removeValue(req)

    //Reactivating darp constraints and triggering propagation:
    for (v <- ttConstraints.indices) {
      cumulConstraints(v).activate()
      cumulConstraints(v).propagate()
      ttConstraints(v).activate()
      ttConstraints(v).propagate()
      distConstraints(v).activate()
      distConstraints(v).propagate()
      for(r <- requests.indices){
        precConstraints(r)(v).activate()
        precConstraints(r)(v).propagate()
      }
    }
    solver.propagate()
  }*/


  /* Search */

  def isDecided(stop: Int): Boolean = {
    if(siteVehicle(stop).isBound)
      sequences(siteVehicle(stop).value).isMember(stop)
    else
      false
  }

  def nInsertionsPossible(i: Int): Int = siteVehicle(i).map(s => {
    sequences(s).nCurrentInsertionsFor(i)
  }).sum

  //Return possible insertions for stop: (seq, elem, pred)
  def computeInsertions(i: Int): Seq[(Int, Int, Int)] = siteVehicle(i).filterNot(_ == nVehicle).flatMap(seq => {
    sequences(seq).allCurrentInsertionsFor(i).map(pred => (seq, i, pred))
  }).toSeq

  search {
    val undecidedElems = sites.indices.filterNot(isDecided) //Filtering undecided stops
    if(undecidedElems.isEmpty) noAlternative //If all stops decided => solution
    else{
      val selectStop = undecidedElems.minBy(nInsertionsPossible) //Selecting stop with less insertions possible
      val inserts = computeInsertions(selectStop)
      if(inserts.isEmpty) branchOne(throw Inconsistency) //If no insertion possible for stop => Inconsitency
      else branchAll(inserts){
        case (seq, elem, pred) => sequences(seq).insertAfter(elem, pred)
      }
    }
  }

  /*var remainTime: Long = 60 * 1000L
  def lns(d: Double): Unit ={
    while(remainTime > 0){
      val stats = solver.startSubjectTo(1) {
        requestRelax(d)
      }
      remainTime -= stats.time
    }
  }

  lns(0.3)*/

  println(start(3))













  protected case class Stop(
                             place: Int,
                             x: Double,
                             y: Double,
                             service: Int,
                             load: Int,
                             winStart: Int,
                             winEnd: Int
                           ){

    def isDepot: Boolean = load == 0

    def isStop: Boolean = !isDepot

    def isPickup: Boolean = load > 0

    def isDrop: Boolean = !isPickup

    def travel: Int = if(isDepot) -1 else if(place > nRequests) place - 1 -nRequests else place - 1
  }

  protected case class DARPRequest(pickup: Int, drop: Int, rideTime: Int)



}
