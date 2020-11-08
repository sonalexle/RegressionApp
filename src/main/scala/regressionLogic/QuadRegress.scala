package regressionLogic

final class QuadRegress() extends Regr {

  private[this] var alpha = 0.0 // slope
  private[this] var beta = 0.0 // intercept
  private[this] var gamma = 0.0
  private[this] var x_sum, y_sum, x2_sum, x3_sum, x4_sum = 0.0
  private[this] var x2y_sum, xy_sum = 0.0

  def update_sums(x: Double, y: Double) = {
    val n1 = n
    n += 1
    val dy = y - meany
    val dy_n = dy / n
    ss_tot += dy * dy_n * n1
    meany += dy_n
    x_sum += x
    y_sum += y
    xy_sum += x * y
    val x2 = x * x
    x2y_sum += x2 * y
    x2_sum += x2
    val x3 = x2 * x
    x3_sum += x3
    x4_sum += x3 * x
  }

  def coefficient = {
    if (gamma == 0 || beta == 0 || alpha == 0) {
      val sx2y = x2y_sum - x2_sum * y_sum / n
      val sxx = x2_sum - x_sum * x_sum / n
      val sxy = xy_sum - x_sum * y_sum / n
      val sxx2 = x3_sum - x_sum * x2_sum / n
      val sx2x2 = x4_sum - x2_sum * x2_sum / n
      gamma = (sx2y * sxx - sxy * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
      beta = (sxy * sx2x2 - sx2y * sxx2) / (sxx * sx2x2 - sxx2 * sxx2)
      alpha = (y_sum - beta * x_sum - gamma * x2_sum) / n
    }
    Vector(gamma, beta, alpha)
  }

  def update_metrics(x: Double, y: Double) = {
    if (gamma == 0 || beta == 0 || alpha == 0) coefficient
    val y_pred = gamma * x * x + beta * x + alpha
    se += (y - y_pred) * (y - y_pred)
    ss_reg += (y_pred - meany) * (y_pred - meany)
  }

}