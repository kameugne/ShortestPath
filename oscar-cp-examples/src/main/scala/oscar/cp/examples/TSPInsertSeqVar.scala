package oscar.cp.examples

import oscar.algo.Inconsistency
import oscar.cp._
import oscar.cp.constraints.sequence.{First, Last, MaxDistanceSOP, Precedence}
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPInsertSeqVar
import oscar.util._

import scala.io.Source

object TSPInsertSeqVar extends CPModel with App{
  val data = TSPparser.parser("Data/TSP/renA20.tsp")
  val n = data.transition.length
  val nRequest = n/2
  val transition = data.transition
  val startDepos = n
  val endDepos = n+1

  val transitionDepos: Array[Array[Int]] = Array.tabulate(n+2,n+2)((i,j) => 0)
  for(i <- 0 to n+1; j <- 0 to n+1){
    if(i < n && j < n) transitionDepos(i)(j) = transition(i)(j)
  }
  val Cities = 0 to n+1

  // variable
  val sequence = CPInsertSeqVar(n+2)
  val totDist = CPIntVar(0 until transitionDepos.flatten.sum)

  // constraintes
  //Setting depots:
  add(First(sequence, startDepos))
  add(Last(sequence, endDepos))

  // precedence
  val precConstraints: Seq[Precedence] = (0 until nRequest).map(r => Precedence(sequence, r, r+nRequest, dependent = true))
  add(precConstraints)


  //Distance constraints:
  add(MaxDistanceSOP(sequence, totDist, transitionDepos), CPPropagStrength.Weak)



  // objective
  minimize(totDist)

  def succ(pred: Int): Int ={
    sequence.allMembers(sequence.allMembers.indexOf(pred)+1)
  }

  // search
  search{
    val unInsert = Cities.filterNot(i => sequence.isMember(i))
    if(unInsert.isEmpty) noAlternative
    else{
      val lessInsertion = unInsert.minBy(i => sequence.nCurrentInsertionsFor(i))

      val insert: Array[(Int,Int, Int)]= sequence.allCurrentInsertionsFor(lessInsertion).map(pred => (pred,lessInsertion, succ(pred))).toArray
      //println(unInsert.mkString("- ") +    "    " + lessInsertion + "     " +insert.mkString(" - ") + "    " + sequence.allMembers.mkString(" - "))
      if(insert.isEmpty) branchOne(throw Inconsistency)
      else branchAll(insert){
          case (pred, elem, succ) => sequence.insertAfter(elem, pred)
      }
    }
  }

  onSolution {
    println("Total distance :" + totDist.value.toDouble/100)
    println(sequence.mkString(" - "))
    println("--------------------------------------")
  }
  println(start())









}

case class TSPInstance(transition: Array[Array[Int]])
object TSPparser{
  final val SCALING = 100
  def parser(filename: String): TSPInstance={
    var lines = Source.fromFile(filename).getLines().toArray
    lines = lines.take(lines.size-1) // drop EOF
    val n = lines(3).split(":")(1).trim().toInt
    val cord = new Array[(Int, Int)](n)
    for(i <- 6 until 6+n){
      val splitted = lines(i).trim.split("\\s+")
      assert(splitted.length == 3)
      cord(i-6) = (splitted(1).toInt,splitted(2).toInt)
    }
    val transition : Array[Array[Int]]= Array.ofDim[Int](n,n)
    for(i <- 0 until n; j <- 0 until n){
      transition(i)(j) = (Math.sqrt( (cord(i)._1 - cord(j)._1)*(cord(i)._1 - cord(j)._1) + (cord(i)._2 - cord(j)._2)*(cord(i)._2 - cord(j)._2))* SCALING).round.toInt
    }
    //println(cord.mkString(" - "))
    TSPInstance(transition)
  }
}