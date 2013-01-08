package oscar.cbls.invariants.lib.numeric

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.{IntSetVar, IntInvariant, IntSetInvariant, IntVar}
import oscar.cbls.invariants.lib.logic.LazyIntVarIntVar2IntVarFun

/**Maintains output to the smallest value such that
 * output >= from
 * (output - shift) MOD period > zone
 * (output - shift + length) MOD period > zone
 * of course, it is required that length is < period - zone, and exception is thrown otherwise.
 */
case class RoundUpModulo(from: IntVar, length: IntVar, period: Int, zone: Int, shift: Int)
  extends LazyIntVarIntVar2IntVarFun(from, length, (from: Int, to: Int) => {
    assert(length.value < period - zone)
    val reducedfrom = (from - shift) % period
    if (reducedfrom < zone)
      from + (zone - reducedfrom) //to restore the modulo, we must compute this
    else if (reducedfrom + length.value > period)
      from + (period + zone - reducedfrom)
    else
      from
  }, from.MinVal, from.MaxVal + zone)


case class RoundUpCustom(from: IntVar, length: IntVar, Zone: List[(Int, Int)]) extends IntInvariant {

  def myMax = Zone.maxBy(_._2)._2 + 1

  def myMin = from.MinVal

  var output: IntVar = null
  registerStaticAndDynamicDependenciesNoID(from, length)
  finishInitialization()

  override def setOutputVar(v: IntVar) {
    output = v
    output.setDefiningInvariant(this)
    output := roundup()
  }

  @inline
  override def notifyIntChanged(v: IntVar, OldVal: Int, NewVal: Int) {
    scheduleForPropagation()
  }

  override def performPropagation() {
    output := roundup()
  }

  def roundup(): Int = {
    var NewStart: Int = from.value
    var LastZoneBeforeNewStart = FindLastStartBefore(from.value)
    while (true) {
      if (!(LastZoneBeforeNewStart == -1 || ForbiddenEnds(LastZoneBeforeNewStart) < from)) {
        //problème de type 1
        NewStart = ForbiddenEnds(LastZoneBeforeNewStart) + 1
        LastZoneBeforeNewStart += 1
      } else if ((LastZoneBeforeNewStart + 1 < ForbiddenStarts.size) && (ForbiddenStarts(LastZoneBeforeNewStart + 1) > from.value + length.getValue())) {
        //problème de type 2
        NewStart = ForbiddenEnds(LastZoneBeforeNewStart + 1) + 1
        LastZoneBeforeNewStart += 1
      } else {
        return NewStart
      }
    }
    1
  }

  val SortedRegularizedZones = regularizeZoneList(Zone.sortWith(_._1 < _._1))

  def regularizeZone(SortedZoneList: List[(Int, Int)], CurrentZoneStart: Int, CurrentZoneEnd: Int): List[(Int, Int)] =
    SortedZoneList match {
      case (s, e) :: tail =>
        if (s > CurrentZoneEnd) ((CurrentZoneStart, CurrentZoneEnd)) :: regularizeZone(tail, s, e)
        else if (e <= CurrentZoneEnd) regularizeZone(tail, CurrentZoneStart, CurrentZoneEnd)
        else regularizeZone(tail, CurrentZoneStart, e)
      case nil => List((CurrentZoneStart, CurrentZoneEnd))
    }

  def regularizeZoneList(SortedZoneList: List[(Int, Int)]): List[(Int, Int)] =
    SortedZoneList match {
      case (s, e) :: tail => regularizeZone(tail, s, e)
      case nil => List.empty
    }

  val ForbiddenStarts: Array[Int] = SortedRegularizedZones.map(_._1).toArray
  val ForbiddenEnds: Array[Int] = SortedRegularizedZones.map(_._2).toArray

  def FindLastStartBefore(d: Int): Int = {
    var up = ForbiddenStarts.size - 1
    var down = -1
    while (down + 1 < up) {
      val mid = (up + down) / 2
      if (ForbiddenStarts(mid) == d) {
        return mid
      } else if (ForbiddenStarts(mid) < d) {
        down = mid
      } else {
        up = mid
      }
    }
    if (ForbiddenStarts(up) <= d) up
    else down
  }
}