package regressionGui

import scala.swing._
import java.awt.{ Graphics2D, Color, RenderingHints }
import scala.math._
import regressionLogic._
import regressionIo._
import scala.swing.BorderPanel.Position._
import scala.collection.mutable.ArrayBuffer

/**
 * The "Plotting" part of the project. Like a graph canvas.
 */
class GraphPanel(private val model: Regr) extends Panel {

  preferredSize = new Dimension(500, 500)

  private val (minx, miny, maxx, maxy, meanx, meany) = model.dataInfo
  private val coef = model.coefficient
  private val dataset = model.getData
  private val w, h = 500
  private[this] var _range: Double = max(maxx - minx, maxy - miny)
  private[this] var _scale: Double = log(_range) * 20
  private[this] var _offsetX = (-meanx * w / _range - 1L).toLong
  private[this] var _offsetY = (meany * h / _range - 1L).toLong
  private val _offsetXOld = _offsetX
  private val _offsetYOld = _offsetY
  private val _origscale = _scale
  private[this] var stepSize = calcInterval(_range)
  private[this] var (limleft, limtop) = toNormalCoord(0, 0)
  private[this] var (limright, limbot) = toNormalCoord(w, h)
  private[this] var _dotsColor: Color = Color.red
  private[this] var _curveColor: Color = Color.blue

  /**
   * Transforms a point from the coordinate system that the computer uses
   * to the Cartesian system.
   */
  private def toNormalCoord(x_coord: Int, y_coord: Int): (Double, Double) = {
    val x = (x_coord - _offsetX - 1 - w / 2.0) * _range / w
    val y = -(y_coord - _offsetY - 1 - h / 2.0) * _range / h
    return (x, y)
  }
  // setters and getters
  def scale = _scale
  def scale_=(r: Double) = {
    val (a, b) = toNormalCoord(w / 2, h / 2) // previous center
    _scale = r
    _range = exp(0.05 * _scale)
    // zooming is always centered
    _offsetX = (-a * w / _range - 1L).toLong
    _offsetY = (b * h / _range - 1L).toLong
  }
  def offsetX = _offsetX
  def offsetX_=(x: Long) = { _offsetX = x }
  def offsetY = _offsetY
  def offsetY_=(y: Long) = { _offsetY = y }
  def origscale = _origscale
  def offsetXOld = _offsetXOld
  def offsetYOld = _offsetYOld
  def curveColor = _curveColor
  def dotsColor = _dotsColor
  def curveColor_=(c: Color) = { _curveColor = c }
  def dotsColor_=(c: Color) = { _dotsColor = c }

  /**
   * Calculates the step sizes for the gridlines
   * and the labels based on the `range`.
   * @param range the range (or `range`) of the axes.
   * @return multiples of 1, 2, or 5.
   */
  private def calcInterval(range: Double): Double = {
    val x = pow(10.0, floor(log10(range)))
    if (range / x >= 5)
      return x
    else if (range / (x * 0.5) >= 5)
      return x / 2
    else return x / 5
  }

  /**
   * For building label strings
   */
  private def numOfDecPlaces(n: Double): Int = {
    val parts = n.toString().split("\\.")
    if (parts.length == 1) 0 else parts(1).length
  }

  /**
   * draws the label `s` if `s` is not zero.
   */
  private def noZero(draw: => Unit, s: String) = {
    if (!s.matches("""0\.?(0)*""")) draw
  }

  private def buildLabel(n: Double, d: Int) = {
    if (n > 1E7 || n < -1E7) f"${n}%.2E"
    else d match {
      case 0 => f"${n}%.0f"
      case 1 => f"${n}%.1f"
      case 2 => f"${n}%.2f"
      case 3 => f"${n}%.3f"
      case 4 => f"${n}%.4f"
      case _ => f"${n}%.5f"
    }
  }

  /**
   * Sets the constants whenever `repaint()` is called
   */
  private def setConst() = {
    limleft = toNormalCoord(0, 0)._1
    limtop = toNormalCoord(0, 0)._2
    limright = toNormalCoord(w, h)._1
    limbot = toNormalCoord(w, h)._2
    stepSize = calcInterval(_range)
  }

  /**
   * Transforms a point from the Cartesian coordinate system
   * to the system that the computer uses.
   */
  private def toMachineCoord(dx: Double, dy: Double): (Int, Int) = {
    var dxx = dx * w / _range + w / 2 + 1L
    var dyy = -dy * h / _range + h / 2 + 1L
    return ((_offsetX + dxx).toInt, (_offsetY + dyy).toInt)
  }

  /**
   * Draws the gridlines (the grey lines) or the labels on the axes.
   * @param vertical an optional parameter. Decides whether to draw
   * horizontal or vertical gridlines (cannot draw both at the same time.)
   * Should only be passed if
   * `isLabel` is `false`.
   * @param isLabel decides whether to draw labels or gridlines.
   */
  private def drawTicks(dx: Double, dy: Double, g: Graphics2D,
                        vertical: Boolean = true, isLabel: Boolean = true): Unit = {
    var (x_coord, y_coord) = toMachineCoord(dx, dy)
    var decPlaces = numOfDecPlaces(stepSize)
    if (decPlaces > 5) decPlaces = 5
    var text = buildLabel({if (dx == 0) dy else dx}, decPlaces)
    if (text(text.length - 1) == '0' && text(text.length - 2) == '.')
      text = text.dropRight(2)
    if (!isLabel) {
      g.setColor(Color.lightGray)
      if (vertical && dx != 0 && 0 <= x_coord && x_coord <= w)
        g.drawLine(x_coord, 0, x_coord, h)
      else if (dy != 0 && 0 <= y_coord && y_coord <= h)
        g.drawLine(0, y_coord, w, y_coord)
    } else {
      val rS = 40 + text.length * 3 / 2
      val bS = 20
      g.setColor(Color.BLACK)
      if (y_coord < 1 && 1 < x_coord && x_coord < w - rS)
        noZero(g.drawString(text, x_coord + 5, 15), text)
      // top border
      else if (y_coord > h - bS && 1 < x_coord && x_coord < w - rS)
        noZero(g.drawString(text, x_coord + 5, h - 5), text)
      // bottom border
      else if (x_coord < 1 && 1 < y_coord && y_coord < h - bS)
        noZero(g.drawString(text, 5, y_coord + 15), text)
      // left border
      else if (x_coord > w - rS && 1 < y_coord && y_coord < h - bS)
        noZero(g.drawString(text, w - text.length * 7, y_coord + 15), text)
      // right border
      else if (1 < y_coord && y_coord < h - bS &&
        1 < x_coord && x_coord < w - rS)
        g.drawString(text, x_coord + 5, y_coord + 15)
      // inside
      else return
    }
  }

  /**
   * Draws a single dot on the canvas. Used to
   * draw the curves and the datapoints.
   * @param r radius of the dot
   */
  private def points(dx: Double, dy: Double, r: Int,
                     g: Graphics2D): Unit = {
    val (x, y) = toMachineCoord(dx, dy)
    g.fillOval(x - r / 2, y - r / 2, r, r)
  }

  /**
   * Controls the `drawTicks` method.
   */
  private def drawxylabels(g: Graphics2D) = {
    var xplus, xminus, yminus, yplus = 0.0
    var xBuf, yBuf = ArrayBuffer[Double]()
    if (xminus > limright + stepSize) // because these start from 0...
      xminus = ceil(limright / stepSize) * stepSize
    if (xplus < limleft - stepSize)
      xplus = floor(limleft / stepSize) * stepSize
    if (yplus < limbot + stepSize)
      yplus = ceil(limbot / stepSize) * stepSize
    if (yminus > limtop - stepSize)
      yminus = floor(limtop / stepSize) * stepSize
    while (xplus < limright || xminus > limleft) {
      xBuf ++= Vector(xplus, xminus)
      xplus += stepSize; xminus -= stepSize
    }
    while (yminus > limbot || yplus < limtop) {
      yBuf ++= Vector(yminus, yplus)
      yminus -= stepSize; yplus += stepSize
    }
    def cond(n: Double, low: Double, high: Double) = {
      low <= n && n <= high
    }
    xBuf = xBuf.filter(cond(_, limleft, limright))
    yBuf = yBuf.filter(cond(_, limbot, limtop))
    xBuf.toSet.foreach((n: Double) => drawTicks(n, 0, g, isLabel = false))
    // draw vertical gridlines
    xBuf.toSet.foreach((n: Double) => drawTicks(n, 0, g))
    // draw numbers along x-axis
    yBuf.toSet.foreach((n: Double) =>
      drawTicks(0, n, g, vertical = false, isLabel = false))
    // draw horizontal gridlines
    yBuf.toSet.foreach((n: Double) => drawTicks(0, n, g, vertical = false))
    // draw numbers along y axis
  }

  /**
   * Draws the graph canvas border and the x-axis
   * and the y-axis.
   */
  private def drawBounds(g: Graphics2D): Unit = {
    g.setColor(Color.LIGHT_GRAY)
    drawxylabels(g)
    g.setColor(Color.BLACK)
    g.drawRect(0, 0, w, h) // border
    if (-w / 2 <= _offsetX && _offsetX <= w / 2)
      g.drawLine(_offsetX.toInt + w / 2, 0, _offsetX.toInt + w / 2, h) // y-axis
    if (-w / 2 <= _offsetY && _offsetY <= w / 2)
      g.drawLine(0, _offsetY.toInt + h / 2, w, _offsetY.toInt + h / 2) // x-axis
  }

  /**
   * Draws a regression line or a parabola depending on
   * how many elements the `coefficient` `Vector` has.
   */
  private def bestFit(g: Graphics2D) = {
    var x = limleft
    var y = 0.0
    val (a, b) = (coef(0), coef(1))
    val step: Double = _range * 0.0003
    g.setColor(_curveColor)
    def draw = if (y >= limbot && y <= limtop) points(x, y, 3, g)
    if (coef.length == 3) {
      val c = coef(2)
      while (x <= limright) {
        y = a * x * x + b * x + c
        draw
        x += step
      }
    } else {
      while (x <= limright) {
        y = a * x + b
        draw
        x += step
      }
    }
  }

  /**
   * Draws all the datapoints in the collected dataset `Vector`.
   */
  private def scatter(g: Graphics2D) = {
    val size = _range match { // select size of the dots
      case i if i <= 40              => 8
      case i if i > 40 && i <= 160   => 7
      case i if i > 160 && i <= 640  => 6
      case i if i > 640 && i <= 2560 => 5
      case i if i > 2560             => 4
    }
    g.setColor(_dotsColor)
    for ((x, y) <- dataset) {
      if (limleft <= x &&
        x <= limright &&
        limbot <= y &&
        y <= limtop)
        points(x, y, size, g)
    }
  }

  override def paintComponent(g: Graphics2D): Unit = {
    g.clearRect(0, 0, size.width, size.height)
    g.setRenderingHint(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)
    setConst()
    drawBounds(g)
    scatter(g)
    bestFit(g)
  }

}

object GraphPanel {

  /**
   * Builds the equation string.
   */
  def eqnBuilder(coef: Seq[Double]): String = {
    coef.length match {
      case 2 => {
        val helper1 = if (coef(1) > 0) "+" else "-"
        val helper2 = if (coef(1) > 0) coef(1) else -coef(1)
        f"y = ${coef(0)}%.5fx ${helper1} ${helper2}%.5f"
      }
      case 3 => {
        def helper1(a: Double) = if (a > 0) "+" else "-"
        def helper2(a: Double) = if (a > 0) a else -a
        val signx2 = helper1(coef(1))
        val x2sign = helper2(coef(1))
        val signx = helper1(coef(2))
        val xsign = helper2(coef(2))
        f"""y = ${coef(0)}%.5fx^2 ${signx2} ${x2sign}%.5fx ${signx} ${xsign}%.5f"""
      }
      case _ => "undefined behavior for other kinds of models"
    }
  }

}