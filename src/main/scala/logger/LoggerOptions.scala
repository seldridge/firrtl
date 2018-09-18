// See LICENSE for license details.

package logger

import firrtl.annotations._
import firrtl.options.{ExecutionOptionsManager, HasScoptOptions, OptionsView}
import firrtl.{AnnotationSeq, FIRRTLException}
import scopt.OptionParser

/** Internal options used to control the FIRRTL compiler
  *
  * @param globalLogLevel the verbosity of logging (default: [[logger.LogLevel.None]])
  * @param classLogLevels the individual verbosity of logging for specific classes
  * @param logToFile      if true, log to a file
  * @param logClassNames  indicates logging verbosity on a class-by-class basis
  */
final case class LoggerOptions(
  globalLogLevel:           LogLevel.Value              = LogLevelAnnotation().globalLogLevel,
  classLogLevels:           Map[String, LogLevel.Value] = Map.empty,
  logToFile:                Boolean                     = false,
  logClassNames:            Boolean                     = false
){

  def getLogFileName: String = ??? // getBuildFileName("log")

  /** Determine if an annotations are sane
    *
    * @param annos a sequence of [[annotation.Annotation]]
    * @return true if all checks pass
    */
  def checkAnnotations(annos: AnnotationSeq): AnnotationSeq = {
    val logLevelAnnotations = annos.collectFirst { case LogLevelAnnotation(x) => x }
    if(logLevelAnnotations.size > 1 ) {
      throw new FIRRTLException(
        s"""|At most one log level can be specified, but found '${logLevelAnnotations.mkString(", ")}' specified via:
            |    - an option or annotation: -ll, --log-level, LogLevelAnnotation""".stripMargin )}
    annos
  }
}

sealed trait LoggerOption extends HasScoptOptions

/** Describes the verbosity of information to log
  *  - maps to [[LoggerOptions.globalLogLevel]]
  *  - set with `-ll/--log-level`
  *  - if unset, a [[LogLevelAnnotation]] with the default log level will be emitted
  * @param globalLogLevel the level of logging
  */
case class LogLevelAnnotation(
  globalLogLevel: LogLevel.Value = LogLevel.None
) extends NoTargetAnnotation with LoggerOption {

  val value: String = globalLogLevel.toString

  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("log-level")
    .abbr("ll")
    .valueName("<Error|Warn|Info|Debug|Trace>")
    .action( (x, c) => c :+ LogLevelAnnotation(LogLevel(x)) )
    .validate{ x =>
      lazy val msg = s"$x bad value must be one of error|warn|info|debug|trace"
      if (Array("error", "warn", "info", "debug", "trace").contains(x.toLowerCase)) { p.success      }
      else                                                                          { p.failure(msg) }}
    .unbounded() // See [Note 1]
    .text(s"Sets the verbosity level of logging, default is ${LoggerOptions().globalLogLevel}")
}

/** Describes a mapping of a class to a specific log level
  *  - maps to [[LoggerOptions.classLogLevels]]
  *  - set with `-cll/--class-log-level`
  * @param className the class name to log
  * @param level the verbosity level
  */
case class ClassLogLevelAnnotation(className: String, level: LogLevel.Value) extends NoTargetAnnotation
  with LoggerOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Seq[String]]("class-log-level")
    .abbr("cll")
    .valueName("<FullClassName:[Error|Warn|Info|Debug|Trace]>[,...]")
    .action( (x, c) => c ++ x.map { y =>
      val className :: levelName :: _ = y.split(":").toList
      val level = LogLevel(levelName)
      ClassLogLevelAnnotation(className, level) } )
    .unbounded() // This can actually occur any number of times safely
    .text(s"This defines per-class verbosity of logging")
}

object ClassLogLevelAnnotation {
  private [logger] def apply(): ClassLogLevelAnnotation = ClassLogLevelAnnotation("", LogLevel.None)
}

/** Enables logging to a file (as opposed to STDOUT)
  *  - maps to [[LoggerOptions.logToFile]]
  *  - enabled with `-ltf/--log-to-file`
  */
case object LogToFileAnnotation extends NoTargetAnnotation with LoggerOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("log-to-file")
    .abbr("ltf")
    .action( (_, c) => c :+ LogToFileAnnotation )
    .unbounded()
    .text(s"default logs to stdout, this flags writes to topName.log or firrtl.log if no topName")
}

/** Enables class names in log output
  *  - maps to [[LoggerOptions.logClassNames]]
  *  - enabled with `-lcn/--log-class-names`
  */
case object LogClassNamesAnnotation extends NoTargetAnnotation with LoggerOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("log-class-names")
    .abbr("lcn")
    .action( (_, c) => c :+ LogClassNamesAnnotation )
    .unbounded()
    .text(s"shows class names and log level in logging output, useful for target --class-log-level")
}

trait HasLoggerOptions { this: ExecutionOptionsManager =>

  parser.note("Logger Options")
  /* [Note 1] Any validation related to these options is removed here. Since
   * we need to check annotations anyway, arguments here are purposefully
   * marked as `unbounded` and no validation checking occurs (except that
   * which is related to ensuring that a command line string is validly
   * converted to some type, e.g., --log-level). All of the actual option
   * validation happens when the annotations are processed in
   * [[FirrtlExecutionUtils.checkAnnotations]]. */
  Seq(
    LogLevelAnnotation(),
    ClassLogLevelAnnotation(),
    LogToFileAnnotation,
    LogClassNamesAnnotation,
  )
  .foreach(_.addOptions(parser))
}

object LoggerViewer {
  implicit object FirrtlOptionsView extends OptionsView[LoggerOptions] {
    def view(options: AnnotationSeq): Option[LoggerOptions] = {
      val opts = options.foldLeft(LoggerOptions()) { (previousOptions, annotation) =>
        annotation match {
          case LogLevelAnnotation(logLevel) => previousOptions.copy(globalLogLevel = logLevel)
          case ClassLogLevelAnnotation(name, level) =>
            val newLogLevels = previousOptions.classLogLevels
            newLogLevels ++ Seq(name, level)
            previousOptions.copy(classLogLevels = newLogLevels)
          case LogToFileAnnotation => previousOptions.copy(logToFile = true)
          case LogClassNamesAnnotation => previousOptions.copy(logClassNames = true)
          case _ => previousOptions
        }
      }
      Some(opts)
    }
  }

  def getView(annotationSeq: AnnotationSeq): LoggerOptions = {
    FirrtlOptionsView.view(annotationSeq).getOrElse(
      throw new FIRRTLException("Unable to determine Logger options for provided command line options and annotations")
    )
  }
}


