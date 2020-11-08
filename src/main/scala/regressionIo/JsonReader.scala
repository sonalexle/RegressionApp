package regressionIo
import java.io.{ File, FileNotFoundException, IOException }
import com.fasterxml.jackson.core._

final class JsonReader extends Reader {

  private[this] var parser: Option[JsonParser] = None
  private[this] var inCol: Option[String] = None
  private[this] var outCol: Option[String] = None

  def init(filename: String, inCol: String, outCol: String) = {
    try {
      if (inCol.trim.isEmpty || outCol.trim.isEmpty)
        throw new NoSuchKeyException("Invalid column name")
      this.inCol = Some(inCol.trim.toLowerCase())
      this.outCol = Some(outCol.trim.toLowerCase())
      parser = Some(new JsonFactory().createParser(new File(filename)))
      val colCheckParser = new JsonFactory().createParser(new File(filename))
      colCheckParser.nextToken()
      if (colCheckParser.currentToken() == null)
        throw new EmptyFileException("Empty file")
      if (colCheckParser.currentToken() != JsonToken.START_ARRAY)
        throw new InvalidFileFormatException("Invalid format")
      colCheckParser.nextToken()
      colCheckParser.nextToken()
      var curr1 = colCheckParser.getCurrentName()
      if (curr1 != null) curr1 = curr1.trim.toLowerCase()
      if (!inCol.equals(curr1) && !outCol.equals(curr1))
        throw new NoSuchKeyException("Invalid column name")
      colCheckParser.nextToken()
      colCheckParser.nextToken()
      var curr2 = colCheckParser.getCurrentName()
      if (curr2 != null) curr2 = curr2.trim.toLowerCase()
      if (!inCol.equals(curr2) && !outCol.equals(curr2))
        throw new NoSuchKeyException("Invalid column name")
      _errorCode = 0
    } catch {
      case e: EmptyFileException                             => _errorCode = 11
      case e: InvalidFileFormatException                     => _errorCode = 4
      case e: NoSuchKeyException                             => _errorCode = 7
      case e: FileNotFoundException                          => _errorCode = 5
      case e: JsonParseException                             => _errorCode = 9
      case e: com.fasterxml.jackson.core.io.JsonEOFException => _errorCode = 15
      case e: IOException                                    => _errorCode = 1
      case _: Throwable                                      => _errorCode = 2
    }
  }

  def readline(): (Double, Double) = {
    var x, y = Double.NaN
    try {
      if (!inCol.isDefined || !outCol.isDefined || !this.parser.isDefined)
        throw new UninitializedException("Uninitialized reader")
      val parser = this.parser.get
      if (parser.nextToken() == JsonToken.END_ARRAY) _errorCode = 14
      else {
        var curr1 = parser.getCurrentName()
        if (curr1 != null) curr1 = curr1.trim.toLowerCase()
        if (inCol.get.equals(curr1) || outCol.get.equals(curr1)) {
          parser.nextToken()
          if (parser.currentToken() != JsonToken.VALUE_NULL) {
            if (inCol.get.equals(curr1)) x = parser.getDoubleValue()
            else y = parser.getDoubleValue()
            parser.nextToken()
            var curr2 = parser.getCurrentName()
            if (curr2 != null) curr2 = curr2.trim.toLowerCase()
            if (inCol.get.equals(curr2) || outCol.get.equals(curr2)) {
              parser.nextToken()
              if (parser.currentToken() != JsonToken.VALUE_NULL)
                if (inCol.get.equals(curr2)) x = parser.getDoubleValue()
                else y = parser.getDoubleValue()
            }
          }
        }
      }
    } catch {
      case e: UninitializedException                         => _errorCode = 12
      case e: com.fasterxml.jackson.core.io.JsonEOFException => _errorCode = 15
      case e: JsonParseException                             => _errorCode = 9
      case _: Throwable                                      => _errorCode = 2
    }
    return (x, y)
  }

}