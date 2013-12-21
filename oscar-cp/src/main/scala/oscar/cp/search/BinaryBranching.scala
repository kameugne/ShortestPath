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

package oscar.cp.search

import oscar.cp.modeling._
import oscar.cp.core.CPVarInt
import oscar.algo.reversible._
import oscar.algo.search.Branching

/**
 * Binary Branching: 
 * You can specify your variable/value heuristics
 * author: Pierre Schaus pschaus@gmail.com
 */
class BinaryBranching(vars: Array[_ <: CPVarInt], varHeuris: (CPVarInt => Int), valHeuris: (CPVarInt => Int) = minVal) extends Branching {
  val cp = vars(0).s
  val x_ = vars.asInstanceOf[Array[CPVarInt]].zipWithIndex
  val nbBounds = new ReversibleInt(cp, 0)
  def bound(i: Int) {
    val ind = nbBounds.value
    val tmp = x_(ind)
    x_(ind) = x_(i)
    x_(i) = tmp
    nbBounds.incr()
  }
  val size = x_.size

  def allBounds(): Boolean = {
    var i = nbBounds.value
    while (i < size) {
      if (!x_(i)._1.isBound) return false
      else bound(i)
      i += 1
    }
    true
  }

  def nextVar(): CPVarInt = {
    var i = nbBounds.value
    var (x, ind) = x_(i)
    var fbest = varHeuris(x)
    i += 1
    while (i < size) {
      if (!x_(i)._1.isBound) {
        val (y, indy) = x_(i)
        val h = varHeuris(y)
        if (h < fbest || (h == fbest && indy < ind)) {
          x = y
          fbest = h
          ind = indy
        }
      } else {
        bound(i)
      }
      i += 1
    }
    x
  }

  def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => { 
        val y = nextVar()
        val v = valHeuris(y)
        branch(cp.assign(y, v))(cp.remove(y, v)) // right alternative
      }
    }
  }
}

class BinaryStaticOrderBranching(vars: Array[_ <: CPVarInt], valHeuris: (CPVarInt => Int) = minVal) extends Branching {

  val cp = vars(0).s
  var y = vars.asInstanceOf[Array[CPVarInt]]
  var i = new ReversibleInt(cp, 0)

  override def alternatives(): Seq[Alternative] = {
    
    while (i.value < y.size && y(i.value).isBound) { i.incr() }
    
    if (i.value < y.size) {

      val x: CPVarInt = y(i.value)
      val v = valHeuris(x)
      branch {
        cp.assign(x, v)
      } {
        cp.remove(x, v)
      }

    } else {
      noAlternative
    }
  }
}


/**
 * Binary First Fail (min dom size) on the decision variables vars.
 * @param vars: the array of variables to assign during the search
 * @param valHeuris: gives the value v to try on left branch for the chosen variable, this value is removed on the right branch
 */
class BinaryFirstFailBranching(x: Array[CPVarInt], valHeuris: (CPVarInt => Int) = minVal) extends BinaryBranching(x, _.size, valHeuris) {
  def this(x: CPVarInt*) = this(x.toArray)
}

/**
 * Binary search on the decision variables vars, selecting first the variables having the max number
 * of propagation methods attached to it.
 */
class BinaryMaxDegreeBranching(x: Array[CPVarInt]) extends BinaryBranching(x, varHeuris = maxDegree, valHeuris = minVal)

/**
 * Binary search on the decision variables vars, splitting the domain of the selected variable on the
 * median of the values (left : <= median, right : > median)
 */
class BinaryDomainSplitBranching(x: Array[CPVarInt], varHeuris: (CPVarInt => Int) = minVar, valHeuris: (Int => Int) = i => i) extends BinaryBranching(x,varHeuris,minVal) {

  override def alternatives(): Seq[Alternative] = {
    allBounds() match {
      case true => noAlternative
      case false => {
        val x = nextVar()
        val vals = x.toArray.sortBy(valHeuris)
        val median = vals(vals.size / 2)

        branch(cp.post(x <= median))(cp.post(x > median))
      }
    }
  }
} 