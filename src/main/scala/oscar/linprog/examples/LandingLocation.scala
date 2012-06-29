/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package oscar.linprog.examples


import oscar.linprog.modeling._
import scala.io.Source

/**
 * Uncapacitated facility location problem.
 * 
 * A series of logs to cut and ship to a set of landings.
 * One must decide which logs to cut, which landings to open, 
 * given shipment and landing opening costs  
 * Cost of shipment of each log to each landing are known.
 * A minimal number of logs should be cut, modeled by a 
 * trade-off coefficient and an additional term in the objective function.
 * 
 * @author Bertrand Cornelusse
 */
object LandingLocation extends MIPModel {

  def main(args: Array[String]) { 
  	
  	// ---------- Data of the Problem ----------
  	
  	val dataIterator = Source.fromFile("data/LandingLocation.txt").getLines
  	val openingCost = for (el <- dataIterator.next.split(";")) yield el.toDouble
  	val transportationCost = (for (line <- dataIterator) yield 
  		for (el <- line.split(";").drop(1)) yield el.toDouble).toArray
  	val Landings = openingCost.indices
  	val Logs = transportationCost.indices
  	val Alpha = 700 // A trade-off coefficient
  	val Demand = 4
  	
  	// ---------- MIP model ----------
  	// The MIP solver
  	val mip = MIPSolver()//glpk lp_solve cplex gurobi
  	// Variables
  	// For each landing either it is ON or OFF
  	val y = for(land <- Landings) yield new MIPVar(mip, "y"+land, 0 to 1)
  	
  	// Transportation decision variables
  	val x = Array.tabulate(Logs.length,Landings.length) ((log,land) => MIPVar(mip,"x"+(log,land), 0 to 1))

  	
  	val obj = sum(Logs,Landings) {(log,land) => x(log)(land)*transportationCost(log)(land)} + sum(Landings) {land => openingCost(land)*y(land)} + Alpha *(Demand - (sum(Logs,Landings) {(log,land) => x(log)(land)*1}))
  	     

  	// Formulation
  	mip.minimize(obj) subjectTo {
  		// One log can be assigned only to one landing
  		for (log <- Logs) {
  		  mip.add(sum(Landings) {(land) => x(log)(land)} <= 1)
  		}
  		// One log can be assigned to a landing only if that landing is open
  		for (log <- Logs; land <- Landings) {
  		  mip.add(x(log)(land) <= y(land))
  		}
  	}
  	
  	println("objective: "+mip.getObjectiveValue())
		println("----------")
		println(y.mkString("\n"))
		x.foreach(log => println(log.map(_.getValue).mkString("\t")))
		x.foreach(log => println(log.map(_.getName).mkString("\t")))
		
		mip.release()
  }
}