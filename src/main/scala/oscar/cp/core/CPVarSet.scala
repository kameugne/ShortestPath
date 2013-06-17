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
package oscar.cp.core

import oscar.reversible.ReversibleQueue
import oscar.reversible.ReversiblePointer
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.Requires
import oscar.cp.constraints.Excludes


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPVarSet(val s: CPStore,min: Int, max: Int, val name: String = "") {

    def store = s
    val dom = new SetDomain(s,min,max);
    

	val onDomainL2 = new ReversiblePointer[ConstraintQueue](s,null)

	val onRequiredL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
	val onExcludedL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
	val onRequiredIdxL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
	val onExcludedIdxL1    = new ReversiblePointer[PropagEventQueueVarSet](s,null)
    
	
    /**
     * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
     */
	def isBound: Boolean = dom.possibleSize == dom.requiredSize
	

	
	/**
     * Test if a value is in the possible values
     * @param val
     * @return  true if value is in the possible values false otherwise
     */
	def isPossibleValue(value: Int) = dom.isPossible(value)

	/**
     * Test if a value is in the required values
     * @param val
     * @return  true if value is in the required values false otherwise
     */
	def isRequiredValue(value: Int) = dom.isRequired(value)	

	
    /**
     * Level 2 registration: ask that the propagate() method of the constraint c is called whenever the domain of the variable changes
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callPropagateWhenDomainChanges(c: Constraint) {
		onDomainL2.setValue(new ConstraintQueue(onDomainL2.value,c));
	}

    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callValRequiredWhenRequiredValue(c: Constraint) {
	  onRequiredL1.setValue(new PropagEventQueueVarSet(onRequiredL1.value,c,this))
	}
	
	
    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */	
	def callValExcludedWhenExcludedValue(c: Constraint) {
	  onExcludedL1.setValue(new PropagEventQueueVarSet(onExcludedL1.value,c,this))
	}
	
    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */
	def callValRequiredIdxWhenRequiredValue(c: Constraint, idx: Int) {
	  onRequiredIdxL1.setValue(new PropagEventQueueVarSet(onRequiredIdxL1.value,c,this,idx))
	}
	
	
    /**
     * Level 1 registration: ask that the propagate() method of the constraint c is called whenever ...
     * @param c
     * @see oscar.cp.core.Constraint#propagate()
     */	
	def callValExcludedIdxWhenExcludedValue(c: Constraint, idx: Int) {
	  onExcludedIdxL1.setValue(new PropagEventQueueVarSet(onExcludedIdxL1.value,c,this,idx))
	}
	
	def requires(v: Int): CPOutcome = {
	  if (dom.isPossible(v) && !dom.isRequired(v)) {
	    // -------- AC3 notifications ------------
        s.notifyL2(onDomainL2.value)
	    // -------- AC5 notifications ------------
        s.notifyRequired(onRequiredL1.value,this,v)
	    s.notifyRequiredIdx(onRequiredIdxL1.value,this,v)
	  }
	  dom.requires(v)
	}
	
	def excludes(v: Int): CPOutcome = {
	  if (dom.isPossible(v) && !dom.isRequired(v)) {
	    // -------- AC3 notifications ------------
        s.notifyL2(onDomainL2.value)
	    // -------- AC5 notifications ------------
        s.notifyExcluded(onExcludedL1.value,this,v)
	    s.notifyExcludedIdx(onExcludedIdxL1.value,this,v)
	  }
	  dom.excludes(v)
	}
	
	def value(): Set[Int] = dom.requiredSet
	
	def requiredSet(): Set[Int] = dom.requiredSet
	
	def possibleSet(): Set[Int] = dom.possibleSet
	
	
	
	
	def include(v: Int) = new Requires (this,v) 
	def exclude(v: Int) = new Excludes (this,v) 

}

object CPVarSet {
  
  
}
  