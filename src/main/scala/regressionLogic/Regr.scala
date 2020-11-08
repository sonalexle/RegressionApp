package regressionLogic
import scala.collection.mutable.HashSet
import scala.util.Random

abstract class Regr() {

  protected[this] var meany = 0.0
  protected[this] var se = 0.0 // squared error (sum)
  protected[this] var ss_reg, ss_tot = 0.0
  protected[this] var n = 0

  /**
   * Decides whether to update the sums or the metrics.
   * Should update the sums before updating the metrics.
   * @param mode the decision. `true` means the metrics
   * should be updated. `false` means the sums should be updated.
   */
  final def fit(x: Double, y: Double, mode: Boolean = false) = {
    if (!mode) update_sums(x, y)
    else update_metrics(x, y)
  }

  /**
   * @return the R^2 score.
   */
  final def r_squared = ss_reg / ss_tot

  /**
   * @return the root mean squared error
   */
  final def mean_squared_error = Math.sqrt(se / n)

  /**
   * @return the data collected for visualization
   * and viewing in `DataTable`.
   */
  final def getData = Regr.getData

  /**
   * @return the numbers necessary to visualize the data.
   */
  final def dataInfo = Regr.dataInfo

  /**
   * @return the coeffients of this regression model.
   * The first element is the highest-order coefficient,
   * the second is the second highest-order, and so on.
   */
  def coefficient: IndexedSeq[Double]

  /**
   * updates the sums.
   */
  protected def update_sums(x: Double, y: Double): Unit

  /**
   * updates the metrics.
   */
  protected def update_metrics(x: Double, y: Double): Unit

}

object Regr { // stores data (maybe randomly picked) from file

  /**
   * Does not maintain insertion order.
   * This collection is used in DataTable and GraphPanel classes.
   */
  private[this] val dataset = HashSet[(Double, Double)]()

  /**
   * For use with min-max scaler.
   */
  private[this] var _minx, _miny = Double.PositiveInfinity
  private[this] var _maxx, _maxy = Double.NegativeInfinity

  /**
   * Length of the whole dataset.
   * @see [[append]]
   */
  private[this] var n = 0

  /**
   * Executes a piece of code with the given probability.
   * Taken from an assignment round in the Programming 2 course.
   * This is written by me.
   */
  private def doWithProb(prob: Double)(thenAction: => Unit): Unit = {
    require(0.0 <= prob && prob <= 100.0)
    if (Random.nextDouble() <= prob * 0.01) thenAction
  }

  private def getData = dataset.toVector // immutable

  /**
   * updates the minimum and maximum values of this dataset.
   * @see [[minMaxScalerOne]]
   */
  def update_minmax(x: Double, y: Double) = {
    if (x > _maxx) _maxx = x
    if (x < _minx) _minx = x
    if (y > _maxy) _maxy = y
    if (y < _miny) _miny = y
  }

  /**
   * updates the length of the whole dataset.
   */
  def update_length() = { n += 1 }

  /**
   * @return A tuple of useful information about the collected
   * dataset. For use with GraphPanel class.
   * First element is min of x-variable.
   * Second is min of y-variable.
   * Third is max of x-variable
   * Fourth is max of y-variable
   * Fifth is mean of x-variable
   * Sixth is mean of y-variable
   */
  private def dataInfo = {
    var minXDS, minYDS = Double.PositiveInfinity
    var maxXDS, maxYDS = Double.NegativeInfinity
    var xsum, ysum, len = 0.0
    for ((x, y) <- dataset) {
      xsum += x
      ysum += y
      len += 1
      if (x > maxXDS) maxXDS = x
      if (x < minXDS) minXDS = x
      if (y > maxYDS) maxYDS = y
      if (y < minYDS) minYDS = y
    }
    (minXDS, minYDS, maxXDS, maxYDS, xsum / len, ysum / len)
  }

  /**
   * Appends a new datapoint to the `dataset` with restrictions.
   */
  def append(x: Double, y: Double, mode: Boolean = false) = {
    if (dataset.size < 1000 && !mode) {
      doWithProb(50){ dataset += (x -> y) }
    } else if (n > 1000000 && mode)
      doWithProb(1){ dataset += (x -> y) }
    else if (mode) dataset += (x -> y)
  }

  def minx = _minx

  def maxx = _maxx

  def miny = _miny

  def maxy = _maxy

  /**
   * Resets this object. Should be called
   * whenever an old `Regr` object is gone.
   */
  def clear() = {
    n = 0
    _minx = Double.PositiveInfinity
    _miny = Double.PositiveInfinity
    _maxx = Double.NegativeInfinity
    _maxy = Double.NegativeInfinity
    dataset.clear()
  }

}