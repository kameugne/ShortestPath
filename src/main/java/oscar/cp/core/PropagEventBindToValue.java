/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.core;

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class PropagEventBindToValue extends PropagEvent {
	
	private CPVarInt var;
	
	public PropagEventBindToValue(Constraint cstr,CPVarInt var) {
		super(cstr);
		this.var = var;
	}
	
	int getPrior() {
		return cstr.getPriorityBindL1();
	}

	@Override
	public CPOutcome notifyConstraint() {
		return cstr.valBind(var);
	}

}