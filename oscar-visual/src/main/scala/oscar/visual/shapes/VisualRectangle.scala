/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.visual.shapes

import java.awt.Color
import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import javax.swing.JInternalFrame
import oscar.visual.VisualDrawing
import oscar.visual.VisualFrame
import oscar.visual.VisualUtil

/**
 *
 * @author Pierre Schaus
 *
 */
class VisualRectangle(d: VisualDrawing, s: Rectangle2D.Double) extends VisualShape(d) {

  type S = Rectangle2D.Double
  protected val shape = s

  def rect: Rectangle2D.Double = shape

  def this(d: VisualDrawing, x: Double, y: Double, w: Double, h: Double) {

    this(d, new Rectangle2D.Double(x, y, w, h))
  }

  /**
   * X coordinates of bottom left corner
   * @return
   */
  def x = rect.getX()

  /**
   * Y coordinates of bottom left corner
   * @return
   */
  def y = rect.getY()

  /**
   * Move the specified left corner
   * @param x
   */
  def move(x: Double, y: Double): Unit = {
    rect.setRect(x, y, width, height)
    drawing.repaint()
  }

  /**
   * width of the rectangle
   * @return
   */
  def width = rect.getWidth()

  /**
   * height of the rectangle
   * @return
   */
  def height = rect.getHeight()

  /**
   * Set width
   * @param w
   */
  def width_=(w: Double): Unit = {
    rect.setRect(x, y, w, height)
    drawing.repaint()
  }

  /**
   * Set height
   * @param w
   */
  def height_=(h: Double): Unit = {
    rect.setRect(x, y, width, h)
    drawing.repaint()
  }

}

object VisualRectangle {

  def main(args: Array[String]): Unit = {


    val f = VisualFrame("toto");
    val d = VisualDrawing(false);
    val inf = f.createFrame("Drawing");
    val toolBar = f.createToolBar()
    inf.add(d);
    f.pack();

    val rect = new VisualRectangle(d, 50, 50, 100, 50);
    rect.toolTip = "Hello";



  }
}
