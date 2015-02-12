/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.CBLSIntVar.int2IntVar
import oscar.cbls.invariants.core.computation.IntInvariant.toIntVar
import oscar.cbls.invariants.lib.minmax.Max2
import oscar.cbls.modeling.Algebra.InstrumentIntInvariant
import oscar.cbls.modeling.Algebra.InstrumentIntVar

/**
 * @author yoann.guyot@cetic.be
 * THIS IS EXPERIMENTAL
 */
class ActivityWithDeadline(
  duration: CBLSIntVar,
  planning: Planning with Deadlines,
  name: String = "",
  shifter: (CBLSIntVar, CBLSIntVar) => CBLSIntVar = (a: CBLSIntVar, _) => a)
  extends Activity(duration, planning, name, shifter) {

  val tardiness = CBLSIntVar(planning.model, Int.MinValue, Int.MaxValue, 0, name + "_tardiness")

  planning.addActivityWithDeadline(this)

  def setDeadline(deadline: CBLSIntVar, weight: CBLSIntVar) {
    tardiness <== Max2(0, (earliestEndDate - deadline) * weight)
  }

  def isLate: Boolean = tardiness.value > 0
}
