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

package oscar.cp.constraints.sequence

import oscar.cp.core._
import oscar.cp.core.variables._

class Excludes(val seq: CPSeqVar, elem: Int) extends Constraint(seq.store, "Seq excludes") {
  override def associatedVars(): Iterable[CPVar] = Array(seq)

  override def setup(l: CPPropagStrength): Unit = seq.excludes(elem)
}

class ExcludeElem(seq: CPSeqVar, elem: Int, b: CPBoolVar) extends Constraint(seq.store, "Seq ExcludedElem") {

  override def associatedVars(): Iterable[CPVar] = Array(seq, b)

  override def setup(l: CPPropagStrength): Unit = {
    propagate()
    if (isActive) {
      seq.callPropagateWhenDomainChanges(this)
      b.callValBindWhenBind(this)
    }
  }

  override def propagate(): Unit = {
//    println("excludes: " + elem)
    if (b.isBound) valBind(b)
    else if (seq.isRequired(elem)) setFalse()
    else if (seq.isExcluded(elem)) setTrue()
  }

  @inline
  private def setTrue(): Unit = {
    b.assign(1)
    deactivate()
  }

  @inline
  private def setFalse(): Unit = {
    b.assign(0)
    deactivate()
  }

  @inline
  private def requires(elem: Int): Unit = {
    seq.requires(elem)
    deactivate()
  }

  @inline
  private def excludes(elem: Int): Unit = {
    seq.excludes(elem)
    deactivate()
  }

  override def valBind(cpVar: CPIntVar): Unit = {
    if (b.isTrue) excludes(elem)
    else requires(elem)
  }
}

object Excludes {
  def apply(seq: CPSeqVar, elem: Int, reifBool: CPBoolVar): Constraint = new ExcludeElem(seq, elem, reifBool)

  def apply(seq: CPSeqVar, elem: Int): Constraint = new Excludes(seq, elem)
}