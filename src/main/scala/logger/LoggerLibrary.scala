// See LICENSE for license details.

package logger

import firrtl.AnnotationSeq
import firrtl.options.{HasScoptOptions, RegisteredLibrary}
import scopt.OptionParser

class LoggerLibrary extends RegisteredLibrary {
  val name: String = "logger"
  override def addOptions(parser: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      LogLevelAnnotation(),
      ClassLogLevelAnnotation(),
      LogToFileAnnotation,
      LogClassNamesAnnotation
    )

    seq.foreach(_.addOptions(parser))
  }
}