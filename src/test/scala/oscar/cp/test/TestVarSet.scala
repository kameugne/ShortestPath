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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._

/**
 * @author: Pierre Schaus pschaus@gmail.com
 */
class TestVarSet extends FunSuite with ShouldMatchers  {


  test("Test Set 1") {
    
    val cp = CPSolver()
    var x = new CPVarSet(cp, -2 , 4)
    var inpropag = false
    class SetCons(val X: CPVarSet) extends Constraint(X.s, "TestSet") {
      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callPropagateWhenDomainChanges(this)
    		return CPOutcome.Suspend
    	}

    	override def propagate(): CPOutcome = {
    	  inpropag = true
          return CPOutcome.Suspend
        }

    }
    

    cp.add(new SetCons(x))
    
    inpropag = false
    cp.add(x.include(0))
    inpropag should be(true)
    
    inpropag = false
    cp.add(x.include(0))
    inpropag should be(false)    
    
    inpropag = false
    cp.add(x.include(-2))
    inpropag should be(true)

    inpropag = false
    cp.add(x.exclude(4))
    inpropag should be(true)

    inpropag = false
    cp.add(x.exclude(4))
    inpropag should be(false)

    inpropag = false
    cp.add(x.exclude(1))
    inpropag should be(true)       

  }
  
  test("Test Set 2") {
    
    val cp = CPSolver()
    var x = new CPVarSet(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
    }
    class SetCons(val X: CPVarSet) extends Constraint(X.s, "TestSet") {

      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedWhenExcludedValue(this)
    	    X.callValRequiredWhenRequiredValue(this)
    		return CPOutcome.Suspend
    	}

    	override def propagate(): CPOutcome = {
    	  inpropag = true
          return CPOutcome.Suspend
        }
    	
    	override def valExcluded(x: CPVarSet, v: Int): CPOutcome = {
    	  inexclude = true
    	  return CPOutcome.Suspend
    	}
    	
    	
    	override def valRequired(x: CPVarSet, v: Int): CPOutcome = {
    	  inrequire = true
    	  return CPOutcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x.include(0))
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    
    reset()
    cp.add(x.include(0))
    inpropag should be(false)    
    inrequire should be(false)
    inexclude should be(false)
    
    
    reset()
    cp.add(x.include(-2))
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)

    reset()
    cp.add(x.exclude(4))
    inpropag should be(true)
    inrequire should be(false)
    inexclude should be(true)

    reset()
    cp.add(x.exclude(4))
    inpropag should be(false)
    inrequire should be(false)
    inexclude should be(false)
    

    reset()
    cp.add(x.exclude(1))
    inpropag should be(true) 
    inrequire should be(false)
    inexclude should be(true)    

  }  
  
  
  test("Test Set 3") {
    
    val cp = CPSolver()
    var x = new CPVarSet(cp, -2 , 4)
    var inpropag = false
    var inexclude = false
    var inrequire = false
    var idx = -100
    var value = -100
    
    def reset() = {
      inpropag = false
      inexclude = false
      inrequire = false
      idx = -100
      value = -100
    }
    class SetCons(val X: CPVarSet) extends Constraint(X.s, "TestSet") {
      
    	override def setup(l: CPPropagStrength): CPOutcome = { 
    	    X.callPropagateWhenDomainChanges(this)
    	    X.callValExcludedIdxWhenExcludedValue(this,100)
    	    X.callValRequiredIdxWhenRequiredValue(this,1000)
    		return CPOutcome.Suspend
    	}

    	override def propagate(): CPOutcome = {
    	  inpropag = true
          return CPOutcome.Suspend
        }
    	
    	override def valExcludedIdx(x: CPVarSet, i: Int, v: Int): CPOutcome = {
    	  inexclude = true
    	  idx = i
    	  value = v
    	  return CPOutcome.Suspend
    	}
    	
    	
    	override def valRequiredIdx(x: CPVarSet, i: Int, v: Int): CPOutcome = {
    	  inrequire = true
     	  idx = i
    	  value = v   	  
    	  return CPOutcome.Suspend
    	}
    }
    

    cp.add(new SetCons(x))
    
    reset()
    cp.add(x.include(0))
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    idx should be(1000)
    value should be(0)
    
    reset()
    cp.add(x.include(0))
    inpropag should be(false)    
    inrequire should be(false)
    inexclude should be(false)
    idx should be(-100)
    value should be(-100)
    
    
    reset()
    cp.add(x.include(-2))
    inpropag should be(true)
    inrequire should be(true)
    inexclude should be(false)
    idx should be(1000)
    value should be(-2)    

    reset()
    cp.add(x.exclude(4))
    inpropag should be(true)
    inrequire should be(false)
    inexclude should be(true)
    idx should be(100)
    value should be(4)     

    reset()
    cp.add(x.exclude(4))
    inpropag should be(false)
    inrequire should be(false)
    inexclude should be(false)
    idx should be(-100)
    value should be(-100)    
    

    reset()
    cp.add(x.exclude(1))
    inpropag should be(true) 
    inrequire should be(false)
    inexclude should be(true)
    idx should be(100)
    value should be(1)      

  }  


}