/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.examples


import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.search._


import scala.math

/**
 *
 * A Dudeney Numbers is a positive integer that is a perfect cube such that the sum of
 * its decimal digits is equal to the cube root of the number.
 * There are only six Dudeney Numbers and those are very easy to find with CP.
 * @author Pierre Schaus pschaus@gmail.com
 */
object Dudeney extends CPModel {

  def main(args: Array[String]) {
    val n = 5

    val cp = new CPSolver()

    val x = (0 until n).map(v => new CPVarInt(cp, 0 to 9))
    val nb = CPVarInt(cp, 1 to math.pow(10, n).toInt - 1)
    val s = CPVarInt(cp, 1 to 9 * n)

    cp.solveAll subjectTo {
      cp.add(nb == (s mul s mul s))
      cp.add(sum(0 until n)(i => x(i) * (math.pow(10, (n - i - 1)).toInt)) == nb)
      cp.add(sum(x) == s)
    } exploration {
      cp.binaryFirstFail(x)
      println(nb.getValue)
    }

    cp.printStats()


  }

}