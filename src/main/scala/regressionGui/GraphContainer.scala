package regressionGui

import scala.swing._
import java.awt.Color
import regressionLogic._
import scala.swing.BorderPanel.Position._
import GridBagPanel._
import scala.swing.event._

/**
 * The panel containing a `GraphPanel`. Acts as the content
 * of a new tab.
 */
class GraphContainer(model: Regr) extends BorderPanel {

  private val canvas = new GraphPanel(model)
  val info = new TextArea {
    editable = false
    text = GraphPanel.eqnBuilder(model.coefficient) +
      {
        if (model.r_squared == 0) ""
        else f", R Squared = ${model.r_squared}%.04f"
      } +
      {
        if (model.mean_squared_error == 0) ""
        else f", RMSE = ${model.mean_squared_error}%.04f"
      }
    font = new Font("Comic Sans MS", java.awt.Font.PLAIN, 18)
  }
  val zoomin = new Button("Zoom in")
  val zoomout = new Button("Zoom out")
  val leftButt = new Button("Left")
  val rightButt = new Button("Right")
  val upButt = new Button("Up")
  val downButt = new Button("Down")
  val cenButt = new Button("Center")
  val curvecolorButt = new Button("Select curve color")
  val dotscolorButt = new Button("Select dots color")

  val infoPane = new FlowPanel {
    background = Color.white
    contents += info
  }

  val botPane = new GridBagPanel {
    border = Swing.EmptyBorder(20, 0, 0, 0)
    background = Color.white
    val c = new Constraints
    c.weighty = 1
    c.weightx = 1
    c.gridx = 0
    c.gridy = 0
    c.gridwidth = 1
    c.fill = Fill.Both
    layout(zoomin) = c
    c.gridx += 1
    layout(zoomout) = c
    c.gridx = 0
    c.gridy += 1
    c.gridwidth = 2
    c.fill = Fill.Horizontal
    layout(curvecolorButt) = c
    c.gridy += 1
    layout(dotscolorButt) = c
  }

  val controls = new GridBagPanel {
    background = Color.white
    val c = new Constraints
    c.weighty = 1
    c.weightx = 1
    c.gridx = 1
    c.gridy = 0
    c.fill = Fill.Both
    layout(upButt) = c
    c.gridy += 1
    c.gridx = 0
    layout(leftButt) = c
    c.gridx += 1
    layout(cenButt) = c
    c.gridx += 1
    layout(rightButt) = c
    c.gridy += 1
    c.gridx = 1
    layout(downButt) = c
  }

  val rightPanel = new BoxPanel(Orientation.Vertical) {
    background = Color.white
    border = Swing.EmptyBorder(0, 0, 90, 0)
    contents ++= Vector(controls, botPane)
  }

  private[this] var mousePt: Point = new Point(0, 0)
  private[this] var dx, dy = 0
  // helper variables for the block of code below
  private[this] var oldX, oldY = 0
  private[this] var movedLeft, movedRight, movedUp, movedDown = false
  private[this] var oldLeft, oldRight, oldUp, oldDown = false
  private[this] var dirXchanged, dirYchanged = false
  // helper end
  background = Color.white
  val margin = (800 - 600 + 100) / 2 - 100
  border = Swing.MatteBorder(10, margin, 0, 0, Color.white)
  layout(canvas) = Center
  layout(infoPane) = South
  layout(rightPanel) = East
  listenTo(canvas.mouse.wheel, canvas.mouse.moves, canvas.mouse.clicks)
  listenTo(zoomin, zoomout, leftButt, rightButt, upButt, downButt, cenButt)
  listenTo(dotscolorButt, curvecolorButt)
  reactions += {
    case ButtonClicked(elem) if elem == curvecolorButt => {
      canvas.curveColor =
        ColorChooser.showDialog(this, "Choose a color",
          canvas.curveColor)
          .getOrElse(canvas.curveColor)
      canvas.repaint()
    }
    case ButtonClicked(elem) if elem == dotscolorButt => {
      canvas.dotsColor =
        ColorChooser.showDialog(this, "Choose a color",
          canvas.dotsColor)
          .getOrElse(canvas.dotsColor)
      canvas.repaint()
    }
    case ButtonClicked(elem) => {
      if (elem == zoomin && canvas.scale > -493) canvas.scale -= 10
      if (elem == zoomout && canvas.scale < 9990) canvas.scale += 10
      if (elem == leftButt) canvas.offsetX += 100L
      if (elem == rightButt) canvas.offsetX -= 100L
      if (elem == upButt) canvas.offsetY += 100L
      if (elem == downButt) canvas.offsetY -= 100L
      if (elem == cenButt) {
        canvas.scale = canvas.origscale
        canvas.offsetX = canvas.offsetXOld
        canvas.offsetY = canvas.offsetYOld
      }
      canvas.repaint()
    }
    case wheel: MouseWheelMoved => {
      val rot = wheel.rotation
      if (rot < 0 && canvas.scale > -493) {
        canvas.scale += rot
      } else if (rot > 0 && canvas.scale < 9990) {
        canvas.scale += rot
      }
      canvas.repaint()
    }
    case drag: MouseDragged => {
      // this block of code detects if mouse has changed direction
      if (oldX > drag.point.x) {
        movedLeft = true; movedRight = false; oldX = drag.point.x
        if (movedLeft != oldLeft) { dirXchanged = true; oldLeft = movedLeft; oldRight = movedRight }
        else { dirXchanged = false }
      }
      if (oldX < drag.point.x) {
        movedRight = true; movedLeft = false; oldX = drag.point.x
        if (movedRight != oldRight) { dirXchanged = true; oldLeft = movedLeft; oldRight = movedRight }
        else { dirXchanged = false }
      }
      if (oldY > drag.point.y) {
        movedUp = true; movedDown = false; oldY = drag.point.y
        if (movedUp != oldUp) { dirYchanged = true; oldUp = movedUp; oldDown = movedDown }
        else { dirYchanged = false }
      }
      if (oldY < drag.point.y) {
        movedDown = true; movedUp = false; oldY = drag.point.y
        if (movedDown != oldDown) { dirYchanged = true; oldDown = movedDown; oldUp = movedUp }
        else { dirYchanged = false }
      }
      // end block
      if (!dirXchanged) dx = drag.point.x - mousePt.x
      else { dx = 0; mousePt = drag.point } // if changed then reset mouse point
      if (!dirYchanged) dy = drag.point.y - mousePt.y
      else { dy = 0; mousePt = drag.point }
      canvas.offsetX += dx / 20
      canvas.offsetY += dy / 20
      canvas.repaint()
    }
    case press: MousePressed => { mousePt = press.point }
  }

}