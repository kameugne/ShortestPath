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
package oscar.algebra

/**Abstract class for variables*/
abstract class Var extends Expression[Linear] {

  def name: String

  val cte = 0.0
  val coef = scala.collection.immutable.Map(this -> 1.0)

  def eval(env: Var => Double) = env(this)
  def value = None

  override def toString = name

//  override def derive(v: Var): Expression = {
//    if (v equals this) One
//    else Zero
//  }

  //def *(cons: Const): Expression[Linear] = new CstVar(cons, this)

  override def equals(that: Any) = {
    that match {
      case other: Var =>
        other.name equals this.name
      case _ => false
    }
  }

  override def hashCode: Int = name.hashCode
}
