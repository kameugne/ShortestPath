/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.constraints;

import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;

/**
 * Absolute value constraint
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Abs extends Constraint {
	
	private CPVarInt x;
	private CPVarInt y;


    /**
     * Build a constraint y = |x|
     * @param x
     * @param y
     */
	public Abs(CPVarInt x, CPVarInt y) {
		super(x.getStore(),"Abs");
		this.x = x;
		this.y = y;
	}

	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		if (y.updateMin(0) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (propagate() == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		if (!x.isBound()) {
			x.callPropagateWhenBoundsChange(this);
			x.callValBindWhenBind(this);
		}
		if (!y.isBound()) {
			y.callPropagateWhenBoundsChange(this);
			y.callValBindWhenBind(this);
		}
		//we can do more propagation with val remove
		return CPOutcome.Suspend;
	}
	
	

	
	@Override
	protected CPOutcome propagate() {
		// y = |x|	
		
		if (x.getMin() >= 0) {
			if (y.updateMin(x.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(x.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(y.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMax(y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		else if (x.getMax() <= 0) {
			if (y.updateMin(-x.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (y.updateMax(-x.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(-y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMax(-y.getMin()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		} else {
			int maxabsy = Math.max(Math.abs(x.getMax()), Math.abs(x.getMin()));			
			if (y.updateMax(maxabsy) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMax(y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			if (x.updateMin(-y.getMax()) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			
		}
		return CPOutcome.Suspend;
	}
	
	@Override
	protected CPOutcome valBind(CPVarInt var) {
		//return CPOutcome.Suspend;
		
		if (x.isBound()) {
			
			if (y.assign(Math.abs(x.getValue())) == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
			return CPOutcome.Success;
			
		} else { // y is bound
			// y = |x|	
			if(!x.hasValue(-y.getValue())) {
				if (x.assign(y.getValue()) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}
			else if(!x.hasValue(y.getValue())) {
				if (x.assign(-y.getValue()) == CPOutcome.Failure) {
					return CPOutcome.Failure;
				}
			}

			else {
				// x can be (y or -y)
				// remove everything except y and -y from x
				for (int v = x.getMin(); v <= x.getMax(); v++) {
					if(v != y.getValue() && v != -y.getValue()) {
						if (x.removeValue(v) == CPOutcome.Failure) {
							return CPOutcome.Failure;
						}
					}
				}
			}
			return CPOutcome.Success;
			
		}
	}
}