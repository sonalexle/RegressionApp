package regressionLogic

/**
 * Helper object for different data preprocessing methods.
 * In the current version of this program, only `minMaxScalerOne`
 * are available to be used.
 */
object Preprocessor {

  private def scale(value: Double, min: Double, max: Double, lower: Double, upper: Double) = {
    lower + ((value - min) * (upper - lower) / (max - min))
  }

  def minMaxScalerOne(x: Double, y: Double,
                      minx: Double, maxx: Double,
                      miny: Double, maxy: Double,
                      lower: Double = -100, upper: Double = 100) = {
    (scale(x, minx, maxx, lower, upper), scale(y, miny, maxy, lower, upper))
  }

}
