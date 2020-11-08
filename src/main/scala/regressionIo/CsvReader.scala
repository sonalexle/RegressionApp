package regressionIo

import java.io.{ BufferedReader, FileReader, FileNotFoundException, IOException }

final class CsvReader(private[this] var delim: String) extends Reader {

  private[this] var csvReader: Option[BufferedReader] = None
  private[this] var idx_in: Int = -1
  private[this] var idx_out: Int = -1

  /**
   * Parses two fields that may contain components of a datapoint.
   * Helper method for `readline`.
   * These two fields come from a line in the CSV file.
   * @param x a field in the `inCol` column.
   * @param y a field in the `outCol` column.
   * @return returns a datapoint (a `Tuple2` of `Double`s) if
   * both of the fields contain valid numbers indicated by the Regex
   * string. If one of the fields contain invalid numbers, or is not numeric,
   * returns a `Tuple2` of `NaN`s.
   */
  private def getDP(x: String, y: String) = { // get data point
    def isNumeric(value: String) = {
      """^-?\d+(\.\d+)?$""".r.findFirstMatchIn(value) match {
        case None    => false
        case Some(_) => true
      }
    }
    if (!x.isEmpty && !y.isEmpty && isNumeric(x) && isNumeric(y))
      (x.toDouble, y.toDouble)
    else (Double.NaN, Double.NaN)
  }

  def init(filename: String, inCol: String, outCol: String): Unit = {
    try {
      if (delim.trim.isEmpty) {
        _errorCode = 6
        return
      }
      if (inCol.trim.isEmpty || outCol.trim.isEmpty)
        throw new NoSuchKeyException("Invalid column name")
      csvReader = Some(new BufferedReader(new FileReader(filename)))
      val lineIn = csvReader.map(_.readLine())
      if (lineIn == Some(null)) throw new EmptyFileException("Empty file")
      val keys = lineIn.get.trim.toLowerCase().split(delim)
      if (!keys.contains(inCol) || !keys.contains(outCol) || keys.isEmpty)
        throw new NoSuchKeyException("Key not found")
      idx_in = keys.indexOf(inCol)
      idx_out = keys.indexOf(outCol)
      _errorCode = 0
    } catch {
      case e: EmptyFileException    => _errorCode = 11
      case e: FileNotFoundException => _errorCode = 5
      case e: NoSuchKeyException    => _errorCode = 7
      case e: IOException           => _errorCode = 1
      case _: Throwable             => _errorCode = 2
    }
  }

  def readline(): (Double, Double) = {
    var x, y = Double.NaN
    try {
      if (!csvReader.isDefined)
        throw new UninitializedException("Uninitialized reader")
      val oneLine = csvReader.get.readLine()
      if (oneLine == null) { _errorCode = 14 }
      else if (oneLine.isEmpty) { _errorCode = 0 }
      else {
        val values = oneLine.split("\\s+").mkString.trim.split(delim, -1)
        _errorCode = 0
        val v = getDP(values(idx_in), values(idx_out))
        x = v._1
        y = v._2
      }
    } catch {
      case e: UninitializedException              => _errorCode = 12
      case e: IOException                         => _errorCode = 2
      case e: java.lang.IndexOutOfBoundsException => _errorCode = 9
      case _: Throwable                           => _errorCode = 2
    }
    return (x, y)
  }

}