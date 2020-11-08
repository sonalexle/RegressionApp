package regressionGui

import regressionLogic._
import regressionIo._
import java.awt.{ Color, Cursor }
import java.io.File
import scala.swing._
import scala.swing.event._
import GridBagPanel._
import javax.swing.{ UIManager, JFileChooser, JPanel }
import javax.swing.filechooser._

/**
 * Main app object. Run this to run the program.
 */
object RegressionApp extends SimpleSwingApplication {

  val fontT = new Font("Times New Roman", java.awt.Font.BOLD, 14)
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  UIManager.put("TextArea.font", fontT)

  val view = new Button("View data") { enabled = false }
  val fitButt = new Button("Fit models")
  val linregrplot = new Button("Linear Plot") { enabled = false }
  val quadregrplot = new Button("Quadratic Plot") { enabled = false }
  val stopButt = new Button("Stop fitting")
  val loadAllY = new RadioButton("Yes") { background = bgcolor }
  val loadAllN = new RadioButton("No") {
    background = bgcolor
    selected = true
  }
  val wantRescale = new CheckBox("Rescale data to [-100, 100] range?") {
    background = bgcolor
    font = fontT
  }
  val inPrompt = new Label("Name of input variable/column") {
    font = fontT
  }
  val inField = new TextField("x", 20)
  val outPrompt = new Label("Name of output variable/column") {
    font = fontT
  }
  val outField = new TextField("y", 20)
  val delimPrompt = new Label("Delimiter of CSV file") {
    font = fontT
  }
  val delimField = new TextField(",", 20)
  val bg = new ButtonGroup() {
    buttons ++= Vector(loadAllY, loadAllN)
  }
  val bgcolor = new Color(245, 245, 220)
  val infotext = new TextArea {
    background = bgcolor
    editable = false
    text = "Select your first action, e.g., open a file!"
  }
  val allButts = Vector(fitButt, view, linregrplot, quadregrplot,
    loadAllY, loadAllN, wantRescale, inField, outField, delimField)
  var fileName: Option[String] = None // None is default file name
  @volatile var running = false
  var linregr: Option[Regr] = None
  var quadregr: Option[Regr] = None
  var run_thread: Thread = null

  val queryPane = new BoxPanel(Orientation.Vertical) {
    background = bgcolor
    val inQuery = new FlowPanel {
      background = bgcolor
      contents ++= Vector(inPrompt, inField)
    }
    val outQuery = new FlowPanel {
      background = bgcolor
      contents ++= Vector(outPrompt, outField)
    }
    val delimQuery = new FlowPanel {
      background = bgcolor
      contents ++= Vector(delimPrompt, delimField)
    }
    val optionsPrompt = new FlowPanel {
      background = bgcolor
      contents += new Label("Visualize all data and see regression metrics?") {
        font = fontT
      }
      contents ++= Vector(loadAllY, loadAllN)
    }
    val rescalePrompt = new FlowPanel {
      background = bgcolor
      contents += wantRescale
    }
    contents += new FlowPanel {
      background = bgcolor
      contents += new Label("User Input") {
        font = fontT
      }
    }
    contents ++= Vector(inQuery, outQuery,
      delimQuery, optionsPrompt, rescalePrompt)
  }

  val buttPane = new BoxPanel(Orientation.Vertical) {
    background = bgcolor
    val infotext = new FlowPanel {
      background = bgcolor
      contents += new Label("Console") {
        font = fontT
      }
    }
    val bg1 = new FlowPanel {
      background = bgcolor
      contents ++= Vector(fitButt, stopButt)
    }
    val bg2 = new FlowPanel {
      background = bgcolor
      contents ++= Vector(linregrplot, quadregrplot)
    }
    val v = new FlowPanel {
      background = bgcolor
      contents += view
    }
    contents ++= Vector(infotext, bg1, bg2, v)
  }

  val consolePane = new SplitPane(
    Orientation.Vertical,
    buttPane, queryPane) {
    enabled = false
    dividerLocation = 400
  }

  val mainPanel = new BorderPanel {
    background = bgcolor
    val pane = new SplitPane(
      Orientation.Horizontal,
      consolePane, infotext) {
      dividerLocation = 400
      enabled = false
    }
    layout(pane) = BorderPanel.Position.Center
  }

  val tabp = new TabbedPane {
    background = Color.white
    preferredSize = new Dimension(900, 700)
  }
  tabp.peer.setOpaque(false)
  tabp.peer.add(mainPanel.peer)
  tabp.peer.setTabComponentAt(
    tabp.peer.indexOfComponent(mainPanel.peer),
    new Label("Console    ").peer)

  lazy val mf = new MainFrame { // mf = mainframe
    background = bgcolor
    title = "Regression"
    resizable = false
    menuBar = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Open")(openFromFile()))
        contents += new Separator()
        contents += new MenuItem(Action("Exit")(closeOperation()))
      }
    }
    override def closeOperation() = {
      if (running) {
        run_thread.interrupt()
        run_thread.join()
      }
      dispose()
    }
    contents = tabp
    listenTo(fitButt, view, stopButt, linregrplot, quadregrplot)
  }

  mf.reactions += {
    case ButtonClicked(elem) if elem == linregrplot =>
      updateView(linregr, tabp)
    case ButtonClicked(elem) if elem == quadregrplot =>
      updateView(quadregr, tabp)
    case ButtonClicked(elem) if elem == view => {
      if (!linregr.isDefined)
        Dialog.showMessage(mf, "You don't have data!", "Tip")
      else {
        val table = new DataTable(
          linregr.get.getData,
          inField.text, outField.text)
        addTab(tabp, table, "Data Table    ")
      }
    }
  }

  mf.reactions += {
    case ButtonClicked(elem) if elem == fitButt => {
      fileName match {
        case None => Dialog.showMessage(
          mf,
          "Please open a file: File>>Open", "Tip")
        case Some(filename) => {
          linregr = Some(new LinRegress())
          quadregr = Some(new QuadRegress())
          Regr.clear()
          changeMouse(mainPanel, false)
          allButts.foreach(_.enabled = false)
          infotext.text = "Started fitting..."
          val reader: Reader = {
            if (filename contains "json") new JsonReader()
            else new CsvReader(delimField.text)
          }
          run_thread = new Thread(new Runnable {
            def run() = {
              running = true
              val epoches = if (loadAllY.selected) {
                if (wantRescale.selected) 3 else 2
              } else if (wantRescale.selected) 2 else 1
              var interrupted: Boolean = false
              reader.init(filename, inField.text.trim.toLowerCase(),
                outField.text.trim.toLowerCase())
              for {
                i <- 0 until epoches
                if reader.errorCode == 0
                if !interrupted
              } {
                while ({
                  interrupted = Thread.interrupted();
                  reader.errorCode == 0 && !interrupted
                }) {
                  val datapoint = reader.readline()
                  val (x, y) = datapoint
                  val (newx, newy) = {
                    if (wantRescale.selected)
                      Preprocessor.minMaxScalerOne(x, y,
                        Regr.minx, Regr.maxx,
                        Regr.miny, Regr.maxy)
                    else datapoint
                  }
                  if (reader.errorCode == 0 &&
                    !Vector(x, y).exists(_.isNaN())) {
                    if (i == 0) {
                      Regr.update_length()
                      if (wantRescale.selected) {
                        Regr.update_minmax(x, y)
                      } else {
                        linregr.foreach(_.fit(x, y))
                        quadregr.foreach(_.fit(x, y))
                        Regr.append(x, y)
                      }
                    } else if (i == 1) {
                      val mode = !wantRescale.selected
                      linregr.foreach(_.fit(newx, newy, mode))
                      quadregr.foreach(_.fit(newx, newy, mode))
                      Regr.append(newx, newy, mode)
                    } else {
                      linregr.foreach(_.fit(newx, newy, true))
                      quadregr.foreach(_.fit(newx, newy, true))
                      Regr.append(newx, newy, true)
                    }
                  }
                }
                if (reader.errorCode == 14) // expected EOF marker
                  reader.init(filename, inField.text.trim.toLowerCase(),
                    outField.text.trim.toLowerCase())
                // if not 14 then some error happened
              }
              running = false
              lazy val onSuccess = {
                if (!interrupted)
                  if (quadregr.get.coefficient.exists(_.isNaN()))
                    infotext.text = "Either no data or too few data (< 3) " +
                      "or wrong column name (json-file)"
                  else infotext.text = eqnDisplay(linregr.get, quadregr.get)
                else infotext.text = "You wanted to stop."
              }
              errorHandler(onSuccess, reader.errorCode, mainPanel)
              changeMouse(mainPanel, true)
              Vector(fitButt, loadAllY, loadAllN, wantRescale,
                inField, outField, delimField).foreach(_.enabled = true)
              if (!quadregr.get.coefficient.exists(_.isNaN()))
                Vector(linregrplot, quadregrplot, view).foreach(_.enabled = true)
            }
          })
          run_thread.start()
        }
      }
    }
  }

  mf.reactions += {
    case ButtonClicked(elem) if elem == stopButt => {
      if (!running) {
        Dialog.showMessage(mf, "Nothing is being fitted!", "Tip")
      } else {
        run_thread.interrupt()
      }
    }
  }

  mf.centerOnScreen()
  mf.pack()

  def top = mf

  /**
   * Changes the mouse to and from a loading mouse.
   */
  def changeMouse(p: Panel, mode: Boolean, text: String = "") = {
    if (mode) p.cursor = Cursor.getDefaultCursor()
    else p.cursor = Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR)
    if (!text.isEmpty) infotext.text = text
  }

  /**
   * Handles file choosing.
   */
  def openFromFile() = {
    val chooser = new JFileChooser(new File("."))
    chooser.setDialogTitle("Select a File")
    chooser.setAcceptAllFileFilterUsed(false)
    chooser.addChoosableFileFilter(new FileNameExtensionFilter(".csv", "csv"))
    chooser.addChoosableFileFilter(new FileNameExtensionFilter(".json", "json"))
    val result = chooser.showOpenDialog(null)
    if (result == JFileChooser.APPROVE_OPTION) {
      val filename = chooser.getSelectedFile().getAbsolutePath()
      if (filename.contains("json") || filename.contains("csv")) {
        fileName = Some(filename.trim)
      }
      infotext.text = s"""Opened ${
        if (filename.length <= 50)
          filename else filename.take(50) + "..."
      }"""
    }
  }

  /**
   * Generates a new plot tab.
   */
  def updateView(model: Option[Regr], tbp: TabbedPane): Unit = {
    model match {
      case Some(m) => {
        if (!m.coefficient.exists(_.isNaN())) {
          val graphpane = new GraphContainer(m)
          val title = if (m.coefficient.length == 3) "QR Plot    " else "LR Plot    "
          addTab(tbp, graphpane.peer, title)
        } else {
          Dialog.showMessage(tbp, "You do not have data!", "Tip")
        }
      }
      case None => Dialog.showMessage(tbp, "Fit a model first!", "Tip")
    }
  }

  /**
   * Adds a new tab to the current `TabbedPane`.
   * Besides from `mainPanel`, there can only 5 more
   * tabs.
   */
  def addTab(tbp: TabbedPane, p: JPanel, title: String) = {
    if (tbp.peer.getTabCount() < 6) {
      tbp.peer.add(p)
      tbp.peer.setTabComponentAt(
        tbp.peer.indexOfComponent(p),
        getTitlePanel(tbp, p, title))
      tbp.revalidate()
      tbp.repaint()
    } else {
      Dialog.showMessage(tbp, "Max number of tabs reached")
    }
  }

  /**
   * Builds a string of two equations from the coefficients of
   * the two regression models.
   */
  def eqnDisplay(model1: Regr, model2: Regr) = {
    GraphPanel.eqnBuilder(model1.coefficient) +
      {
        if (model1.r_squared == 0 &&
          model1.mean_squared_error == 0) "\n\n"
        else f", R Squared = ${model1.r_squared}%.5f" +
          f", RMSE = ${model1.mean_squared_error}%.5f\n\n"
      } +
      GraphPanel.eqnBuilder(model2.coefficient) +
      {
        if (model2.r_squared == 0 &&
          model2.mean_squared_error == 0) ""
        else f", R Squared = ${model2.r_squared}%.5f" +
          f", MSE = ${model2.mean_squared_error}%.5f"
      }
  }

  def errorHandler(successCase: => Unit, ec: Int, panel: Panel) = {
    ec match {
      case 1  => Dialog.showMessage(panel, "Unknown IO error", "Tip")
      case 2  => Dialog.showMessage(panel, "Unknown error during reading file", "Tip")
      case 4  => Dialog.showMessage(panel, "File format invalid", "Tip")
      case 5  => Dialog.showMessage(panel, "File not found", "Tip")
      case 6  => Dialog.showMessage(panel, "Invalid or no delimiter", "Tip")
      case 7  => Dialog.showMessage(panel, "Invalid column name\nor invalid delimiter", "Tip")
      case 9  => Dialog.showMessage(panel, "File either contains unknown\n value or is corrupted", "Tip")
      case 11 => Dialog.showMessage(panel, "File is empty", "Tip")
      case 12 => Dialog.showMessage(panel, "No reader", "Tip")
      case 14 => Dialog.showMessage(panel, "Reached expected EOF", "Tip")
      case 15 => Dialog.showMessage(panel, "Reached unexpected EOF", "Tip")
      case 0  => successCase
      case _  => Dialog.showMessage(panel, "Some error occurred!", "Tip")
    }
    if (ec != 0) infotext.text = "Some kind of error occured in a previous load operation."
  }

  /**
   * Adds the name of the tab and the close button to a new tab.
   * @param tbp the `TabbedPane` to be added the new tab.
   * @param panel the content of the new tab.
   * @param title the title of the new tab.
   * @return a new panel containing the title and the close button.
   * This panel will go in the tab bar.
   */
  def getTitlePanel(tbp: TabbedPane, panel: JPanel, title: String) = {
    val closeButt = new Button("x") {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          tbp.peer.remove(panel)
      }
    }
    val titlePanel = new GridBagPanel {
      val c = new Constraints
      c.gridwidth = 2
      c.fill = Fill.Both
      c.weightx = 1
      val tp = new Label(title) {
        background = Color.white
      }
      layout(tp) = c
      c.gridwidth = 1
      c.gridx = 2
      c.weightx = 0
      layout(closeButt) = c
    }
    titlePanel.peer.setOpaque(false)
    titlePanel.peer
  }

}