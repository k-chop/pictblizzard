package pictbliz
package gui

/*
 * JFreeChart : a free chart library for the Java(tm) platform
 *
 *
 * (C) Copyright 2000-2007, by Object Refinery Limited and Contributors.
 *
 * Project Info:  http://www.jfree.org/jfreechart/index.html
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 *
 * [Java is a trademark or registered trademark of Sun Microsystems, Inc.
 * in the United States and other countries.]
 *
 * -------------
 * SWTUtils.java
 * -------------
 * (C) Copyright 2006, 2007, by Henry Proudhon and Contributors.
 *
 * Original Author:  Henry Proudhon (henry.proudhon AT ensmp.fr);
 * Contributor(s):   Rainer Blessing;
 *                   David Gilbert (david.gilbert@object-refinery.com);
 *                   Christoph Beck.
 *
 * Changes
 * -------
 * 01-Aug-2006 : New class (HP);
 * 16-Jan-2007 : Use FontData.getHeight() instead of direct field access (RB);
 * 31-Jan-2007 : Moved the dummy JPanel from SWTGraphics2D.java,
 *               added a new convert method for mouse events (HP);
 * 12-Jul-2007 : Improved the mouse event conversion with buttons
 *               and modifiers handling, patch sent by Christoph Beck (HP);
 * 27-Aug-2007 : Modified toAwtMouseEvent signature (HP);
 * 27-Nov-2007 : Moved convertToSWT() method from SWTGraphics2D and added
 *               convertAWTImageToSWT() (DG);
 * 01-Jul-2008 : Simplify AWT/SWT font style conversions (HP);
 *
 */

import java.awt.image.BufferedImage
import java.awt.image.DirectColorModel
import java.awt.image.IndexColorModel
import java.awt.image.WritableRaster
import javax.swing.JPanel
import org.eclipse.swt.graphics.ImageData
import org.eclipse.swt.graphics.PaletteData
import org.eclipse.swt.graphics.RGB
import java.lang.String

/**
 * Utility class gathering some useful and general method.
 * Mainly convert forth and back graphical stuff between
 * awt and swt.
 */
object SWTUtils {

  /**
   * Converts a buffered image to SWT <code>ImageData</code>.
   *
   * @param bufferedImage  the buffered image (<code>null</code> not
   *                       permitted).
   *
   * @return The image data.
   */
  def convertToSWT(bufferedImage: BufferedImage): ImageData = {
    if (bufferedImage.getColorModel.isInstanceOf[DirectColorModel]) {
      val colorModel: DirectColorModel = bufferedImage.getColorModel.asInstanceOf[DirectColorModel]
      val palette: PaletteData = new PaletteData(colorModel.getRedMask, colorModel.getGreenMask, colorModel.getBlueMask)
      val data: ImageData = new ImageData(bufferedImage.getWidth, bufferedImage.getHeight, colorModel.getPixelSize, palette)
      val raster: WritableRaster = bufferedImage.getRaster
      val pixelArray: Array[Int] = Array.ofDim[Int](4)
      locally {
        var x = 0; var y = 0
        while(y < data.height){
          while(x < data.width) {
            raster.getPixel(x, y, pixelArray)
            val pixel: Int = palette.getPixel(new RGB(pixelArray(0), pixelArray(1), pixelArray(2)))
            data.setPixel(x, y, pixel)
            x += 1
          }
          y += 1
          x = 0
        }
      }
      data
    } else if (bufferedImage.getColorModel.isInstanceOf[IndexColorModel]) {
      val colorModel: IndexColorModel = bufferedImage.getColorModel.asInstanceOf[IndexColorModel]
      val size: Int = colorModel.getMapSize
      val reds: Array[Byte] = new Array[Byte](size)
      val greens: Array[Byte] = new Array[Byte](size)
      val blues: Array[Byte] = new Array[Byte](size)
      colorModel.getReds(reds)
      colorModel.getGreens(greens)
      colorModel.getBlues(blues)
      val rgbs: Array[RGB] = Array.ofDim[RGB](size)
      locally {
        var i: Int = 0
        while (i < rgbs.length) {
          rgbs(i) = new RGB(reds(i) & 0xFF, greens(i) & 0xFF, blues(i) & 0xFF)
          i += 1
        }
      }
      val palette: PaletteData = new PaletteData(rgbs)
      val data: ImageData = new ImageData(bufferedImage.getWidth, bufferedImage.getHeight, colorModel.getPixelSize, palette)
      data.transparentPixel = colorModel.getTransparentPixel
      val raster: WritableRaster = bufferedImage.getRaster
      val pixelArray: Array[Int] = Array(0)
      locally {
        var x = 0; var y = 0
        while(y < data.height){
          while(x < data.width) {
            raster.getPixel(x, y, pixelArray)
            data.setPixel(x, y, pixelArray(0))
            x += 1
          }
          y += 1
        }
      }
      data
    } else null
  }
}
