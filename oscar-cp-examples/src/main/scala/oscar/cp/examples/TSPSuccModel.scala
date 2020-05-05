package oscar.cp.examples

import oscar.cp._
import oscar.util._
object TSPSuccModel extends CPModel with App{
  val data = TSPparser.parser("Data/TSP/renA50.tsp")
  val n = data.transition.length
  val transition = data.transition
  // Variables
  val succ = Array.fill(n)(CPIntVar(0 until n))
  val totDist = CPIntVar(0 until Int.MaxValue)
  add(sum(0 until n)(i => transition(i)(succ(i))) === totDist)

  // Constraints
  add(minCircuit(succ, transition, totDist), Weak)


  // Search heuristic
  minimize(totDist)

  search {
    // Select the not yet bound city with the smallest number of possible successors
    selectMin(0 until n)(!succ(_).isBound)(succ(_).size) match {
      case None => noAlternative
      case Some(x) => {
        // Select the closest successors of the city x
        val v = selectMin(0 until n)(succ(x).hasValue(_))(transition(x)(_)).get
        branch(add(succ(x) === v))(add(succ(x) !== v))
      }
    }
  }

  onSolution {
    println("Total distance :" + totDist.value.toDouble/100)
    println(succ.mkString(" - "))
    println("--------------------------------------")
  }
  println(start())

}
