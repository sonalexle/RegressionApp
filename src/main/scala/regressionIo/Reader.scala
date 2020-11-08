package regressionIo

/**
 * Base class for file reading.
 */
abstract class Reader() {

  protected[this] var _errorCode: Int = 0

  /**
   * Getter of the `_errorCode` variable.
   * Signifies whether a previous read operation
   * was successful or not.
   * @return If no error occurs
   * (initialization successfull or a datapoint can be read),
   * returns 0. If there was an error, returns a positive
   * integer. Each integer signifies a different kind of error.
   * @see [[errorHandler]] in `RegressionApp`.
   */
  def errorCode = _errorCode

  /**
   * Initializes this `Reader`.
   * @param filename the name of the file to be read.
   * @param inCol the name of the input column/variable.
   * @param outCol the name of the output column/variable.
   */
  def init(filename: String, inCol: String, outCol: String)

  /**
   * Reads a single "line" and returns a single datapoint.
   * Use a loop to read all of the file.
   * Should only be used if initialization was successful,
   * i.e., `errorCode` returns 0.
   */
  def readline(): (Double, Double)

}

/**
 * Will be thrown if a column name (`inCol` or `outCol`)
 * is not found.
 */
class NoSuchKeyException(m: String) extends Exception(m)

/**
 * Will be thrown if a JSON file is not in the specified format.
 */
class InvalidFileFormatException(m: String) extends Exception(m)

/**
 * Will be thrown if `readline` is called before `init`.
 */
class UninitializedException(m: String) extends Exception(m)

/**
 * Will be thrown if file is empty (`init` failed.)
 */
class EmptyFileException(m: String) extends Exception(m)