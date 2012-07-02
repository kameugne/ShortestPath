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

/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{Invariant, IntSetVar, IntVar}
import oscar.cbls.invariants.core.computation.Invariant._
import collection.immutable.SortedSet

/**
 * Maintains a resource usage profile.
 * @param indices the indices of tasks
 * @param start the start time of tasks
 * @param duration the duration of tasks
 * @param amount the amount that tasks use of this resource
 * @param profile the usage profile of the resource maintained to profile(time) <== sum(task.amount | task.start <= time <= t.start+t.duration)
 * @param active the tasks that are active maintained to active(time) <== (task.indices | task.start <= time <= t.start+t.duration)
 */
case class Cumulative(indices:Array[Int], start:Array[IntVar], duration:Array[IntVar], amount:Array[IntVar], profile:Array[IntVar], active:Array[IntSetVar]) extends Invariant {

  for (v <- start.indices) registerStaticAndDynamicDependency(start(v),v)
  for (v <- duration.indices) registerStaticAndDynamicDependency(duration(v),v)
  for (v <- amount.indices) registerStaticAndDynamicDependency(amount(v),v)

  finishInitialization()

  for(v <- profile){v.setDefiningInvariant(this); v := 0}
  for(v <- active ){v.setDefiningInvariant(this); v := SortedSet.empty}
  
  for(i <- start.indices)insert(start(i), duration(i), amount(i), i)

  def remove(start:Int, duration:Int, amount:Int,index:Int){
    for (t <- start until (start + duration)){
      profile(t) :-= amount
      active(t).deleteValue(indices(index))
    }
  }

  def insert(start:Int, duration:Int, amount:Int, index:Int){
    for (t <- start until (start + duration)){
      profile(t) :+= amount
      active(t).insertValue(indices(index))
    }
  }

  @inline
  override def notifyIntChanged(v:IntVar,index:Int,OldVal:Int,NewVal:Int){
    if (start(index) == v){
      //start
      remove(OldVal, duration(index), amount(index), index)
      insert(NewVal, duration(index), amount(index), index)
    }else if (duration(index) == v){
      //duration
      if (OldVal > NewVal){
        remove(NewVal + start(index), OldVal - NewVal, amount(index), index)
      }else{
        insert(OldVal + start(index), NewVal - OldVal, amount(index), index)
      }
    }else{
      //amount
      val Delta = NewVal - OldVal
      for (t <- start(index).getValue() until (start(index).getValue() + duration(index))){
        profile(t) :+= Delta
      }
    }
  }
  //TODO: checkInternals, attention, on n'est pas sur des indexes et alignements divers.
}
