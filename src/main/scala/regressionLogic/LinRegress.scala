package regressionLogic

import scala.collection.mutable
import scala.util.Random

final class LinRegress() extends Regr {

  private[this] var meanx = 0.0
  private[this] var s_var, s_cov = 0.0
  /* not actually variance and covariance
   * but the sums before dividing by population size */
  private[this] var m = 0.0 // slope
  private[this] var c = 0.0 // intercept

  def update_sums(x: Double, y: Double) = {
    val n1 = n
    n += 1
    val dx = x - meanx
    val dy = y - meany
    val dx_n = dx / n
    val dy_n = dy / n
    s_var += dx * dx_n * n1
    ss_tot += dy * dy_n * n1
    meanx += dx_n
    meany += dy_n
    s_cov += dx * (y - meany)
  }

  def coefficient: IndexedSeq[Double] = {
    if (m == 0 || c == 0) {
      m = s_cov / s_var
      c = meany - m * meanx
    }
    Vector(m, c)
  }

  def update_metrics(x: Double, y: Double) = {
    if (m == 0 || c == 0) coefficient
    val y_pred = m * x + c
    se += (y - y_pred) * (y - y_pred)
    ss_reg += (y_pred - meany) * (y_pred - meany)
  }

}