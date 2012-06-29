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
 * Less Than Constraint ( x < y)
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Le extends Constraint {

	private CPVarInt x, y;

    /**
     * x < y
     * @param x
     * @param y
     */
	public Le(CPVarInt x, CPVarInt y) {
		super(x.getStore());
		this.x = x;
		this.y = y;
	}
	
	public Le(CPVarInt x, int v) {
		this(x, new CPVarInt(x.getStore(),v,v));
	}
	
	@Override
	protected CPOutcome setup(CPPropagStrength l) {
		// y > x
		if(s.post(new Gr(y,x)) == CPOutcome.Failure) {
			return CPOutcome.Failure;
		}
		return CPOutcome.Success;
	}
	
}