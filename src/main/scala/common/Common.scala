package common

import scala.io.Source

object Common {

  def loadData(source: String): Seq[String] = {
    Source
      .fromResource(source)
      .getLines()
      .toSeq
  }

}
