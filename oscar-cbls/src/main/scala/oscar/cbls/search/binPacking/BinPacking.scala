/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.cbls.search.binPacking

import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar}
import oscar.cbls.objective.Objective
import oscar.cbls.search.SearchEngineTrait

case class Item(number:Int,
                size:Int,
                bin: CBLSIntVar)

case class Bin(number:Int,
               size:Int,
               var items:CBLSSetVar = null,
               var violation:CBLSIntVar = null)

object BinPackingSolver extends SearchEngineTrait{
  def solveBinPacking(items:Map[Int,Item], bins: Map[Int,Bin], overallViolation:Objective, mostViolatedBins:CBLSSetVar, maxStep:Int) = {
    val swapNB = new SwapItemsNeighborhood(items, bins, overallViolation, mostViolatedBins)
    val moveNB = new ItemMoveNeighborhood(items, bins, overallViolation, mostViolatedBins)
    val compose = moveNB exhaustBack swapNB

    var remainingIt = maxStep;
    while(remainingIt < 0 && compose.doFirstImprovingMove()){
      remainingIt -= 1
    }
  }
}
