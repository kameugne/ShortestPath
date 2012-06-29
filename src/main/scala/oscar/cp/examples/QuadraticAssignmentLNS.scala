/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.examples


import oscar.cp.modeling._
import oscar.cp.search._

import scala.io.Source
import java.lang._
import scala.collection.JavaConversions._

/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object QuadraticAssignmentLNS extends CPModel {
  def main(args: Array[String]) {

    // Read the data
    var lines = Source.fromFile("data/qap.txt").getLines.toList.filter(_ != "")
    val n = lines.head.toInt
    val N = 0 until n
    lines = lines.drop(1)
    var w: Array[Array[Int]] = Array() //weight matrix
    var d: Array[Array[Int]] = Array() //distance matrix
    for (i <- N) {
      w = w :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    for (i <- N) {
      d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }

    // State the model and solve it
    val cp = CPSolver()
    val x = N map (v => CPVarInt(cp, 0 until n))
    val D = Array.tabulate(n, n)((i, j) => element(d, x(i), x(j))) //matrix of variables representing the distances

    val rand = new scala.util.Random(0)
    val bestSol = Array.fill(n)(0)
    
    
    cp.lns(20,50) {
      // relax 50% of the variables
      cp.post((0 until n).filter(i => rand.nextInt(100) < 50).map(i => x(i) == bestSol(i)))
    }
    
    var nbSol = 0
    
    cp.minimize(sum(N, N)((i, j) => D(i)(j) * w(i)(j))) subjectTo {
      cp.add(alldifferent(x), Strong)
    } exploration {
        cp.binaryFirstFail(x)
        println("solution"+x.mkString(","))
        // store the current best solution
        (0 until n).foreach(i => bestSol(i) = x(i).getValue())
        nbSol += 1
        if (nbSol == 100) cp.stop() // stop after the 100th solution
    }

    // Print some statistics
    cp.printStats()
  }

}