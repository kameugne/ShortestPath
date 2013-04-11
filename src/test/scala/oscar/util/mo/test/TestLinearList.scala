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

package oscar.util.mo.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.util.mo.LinearList
import oscar.util.mo.MOOPoint

/**
 * @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
 */
class TestLinearList extends FunSuite with ShouldMatchers {

/*
 * INFO:
 * http://www.r-bloggers.com/ascii-scatterplots-in-r/
 * source("http://biostatmatt.com/R/scat.R")
 * data(co2) #Mauna Loa Atmospheric CO2 Concentration
 * scat(c(co2[1:75]), rows=10, cols=80)
 */
  
// ______________________________________________________
//|                                                      |
//|  * x1=(10,50)                          x6=(50,50) *  |
//|                                                      |
//|                                                      |
//|                                 x5=(45,45) *         |
//|                                                      |
//|                          * x2=(30,40)                |
//|                                                      |
//|                                                      |
//|              * x8=(20,35)                            |
//|                                                      |
//|  * x7=(10,30)                        * x3=(40,30)    |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                                      |
//|                                        x4=(50,10) *  |
//|______________________________________________________|
  
	val x1 = MOOPoint(Array(10,50), Array(10,50))
	val x2 = MOOPoint(Array(30,40), Array(30,40))
	val x3 = MOOPoint(Array(40,30), Array(40,30)) 
	val x4 = MOOPoint(Array(50,10), Array(50,10))
	val x5 = MOOPoint(Array(45,45), Array(45,45))
	val x6 = MOOPoint(Array(50,50), Array(50,50))
	val x7 = MOOPoint(Array(10,30), Array(10,30))
	val x8 = MOOPoint(Array(20,35), Array(20,35))
  
	test("Test LinearList 1") {
		val list = new LinearList(x1)
		list.insert(x2)
		list.insert(x3)
		list.size should be(3)
		list.toSet should be(Set(x1,x2,x3))
	}
	
	test("Test LinearList 2") {
		val list = new LinearList(x4)
		list.insert(x2)
		list.insert(x3)
		list.insert(x1)
		list.insert(x3)
		list.insert(x4)
		list.insert(x1)
		list.size should be(4)
		list.toSet should be(Set(x1,x2,x3,x4))
	}
	
	test("Test LinearList 3") {
		val list = new LinearList(x1)
		list.insert(x5)
		list.insert(x3)
		list.insert(x2)
		list.size should be(2)
		list.toSet should be(Set(x1,x5))
	}
	
	test("Test LinearList 4") {
		val list = new LinearList(x1)
		list.insert(x5)
		list.insert(x3)
		list.insert(x2)
		list.insert(x1)
		list.insert(x5)
		list.insert(x7)
		list.size should be(2)
		list.toSet should be(Set(x1,x5))
	}
	
	test("Test LinearList 5") {
		val list = new LinearList(x1)
		list.insert(x5)
		list.insert(x3)
		list.insert(x2)
		list.insert(x1)
		list.insert(x5)
		list.insert(x7)
		list.insert(x8)
		list.insert(x6)
		list.size should be(1)
		list.toSet should be(Set(x6))
	}
	
	test("Test LinearList 6") {
		val list = new LinearList(x4)
		list.insert(x3)
		list.insert(x8)
		list.insert(x1)
		list.size should be(4)
		list.toSet should be(Set(x4,x3,x8,x1))
		
		list.insert(x7)
		list.size should be(4)
		list.toSet should be(Set(x4,x3,x8,x1))
		
		list.insert(x2)
		list.size should be(4)
		list.toSet should be(Set(x4,x3,x2,x1))
		
		list.insert(x5)
		list.size should be(3)
		list.toSet should be(Set(x4,x5,x1))
	}
	
    test("Test LinearList 7") {
		val list = new LinearList(x3)
		list.insert(x4)
		list.insert(x7)
		list.insert(x8)
		list.size should be(3)
		list.toSet should be(Set(x3,x4,x8))
	}
    
    test("Test LinearList 8") {
		val list = new LinearList(x2)
		list.insert(x4)
		list.insert(x3)
		list.insert(x8)
		list.insert(x1)
		list.size should be(4)
		list.toSet should be(Set(x4,x3,x2,x1))
		
		list.insert(x7)
		list.size should be(4)
		list.toSet should be(Set(x4,x3,x2,x1))
		
		list.insert(x2)
		list.size should be(4)
		list.toSet should be(Set(x4,x3,x2,x1))
		
		list.insert(x5)
		list.size should be(3)
		list.toSet should be(Set(x4,x5,x1))		
	}
	
}