package regressionGui

import javax.swing._
import java.awt._
import java.awt.event._

/**
 * A panel that displays a dataset. Acts as the
 * content of a new tab.
 * @param data the data to be visualized.
 * @param inCol the name of the middle column in the table.
 * @param outCol the name of the right column in the table.
 */
class DataTable(data: Vector[(Double, Double)], inCol: String, outCol: String) extends JPanel {

  private val data_arr = data.toArray.zipWithIndex
    .map(triple => Array(
      new java.lang.Integer(triple._2),
      new java.lang.Double(triple._1._1),
      new java.lang.Double(triple._1._2))) // autoboxing
    .asInstanceOf[Array[Array[java.lang.Object]]]

  private val table = new JTable(
    data_arr,
    Array[java.lang.Object]("# (not original order)", inCol, outCol)) {
    override def isCellEditable(row: Int, column: Int): Boolean = { return false }
    setPreferredScrollableViewportSize(new Dimension(600, 600))
    setFillsViewportHeight(true)
  }

  add(new JScrollPane(table))

  setOpaque(false)

}