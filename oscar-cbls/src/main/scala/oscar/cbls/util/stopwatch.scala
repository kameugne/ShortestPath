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
/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

package oscar.cbls.util

/**This trait implements simple stopwatch functionality used for benchmarking search scripts
  * @author renaud.delandtsheer@cetic.be
  * */
trait StopWatch {

  private var starttime:Long = 0

  /**starts or resets the stopwatch*/
  def startWatch(): Unit ={
    starttime = System.nanoTime()
  }

  /**returns the time elapsed since the last call to startWatch.
   * time is wall clock time in milliseconds
   */
  def getWatch:Long = (System.nanoTime() - starttime)/(1000*1000)

  /**returns a string describing the time elapsed since last startWatch
   * formatted for humans: hh:mm:ss:ms
   */
  def getWatchString:String = {
    val ms1 = getWatch
    val s1 :Long= ms1 / 1000
    val m1 :Long= s1 / 60
    val h : Long = m1 / 60

    val m :Long= m1 - 60*h
    val s :Long= s1 - 60*m1
    val ms:Long = ms1 - 1000*s1

    h + ":" + m + ":" + s + " & " + ms + "ms"
  }
}
