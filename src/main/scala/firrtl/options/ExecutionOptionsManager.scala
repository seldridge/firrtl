// See LICENSE for license details.

package firrtl.options

import firrtl.{AnnotationSeq, FIRRTLException}
import scopt.OptionParser
import java.util.ServiceLoader

/** A store of command line options as an [[AnnotationSeq]]
  *
  * @param applicationName  The name shown in the usage
  * @param args Command line arguments to process
  * @param annotations Initial options to start with
  */
class ExecutionOptionsManager(
  val applicationName: String,
  args: Array[String],
  annotations: AnnotationSeq = AnnotationSeq(Seq.empty)) {

  /** Command line argument parser ([[scopt.OptionParser]]) with modifications */
  implicit final val parser = new OptionParser[AnnotationSeq](applicationName)
      with TerminateOnExit[AnnotationSeq]
      with DuplicateHandling[AnnotationSeq]

  /** By default scopt calls sys.exit when --help is in options, this defeats that */
  def doNotExitOnHelp(): Unit = {
    parser.terminateOnExit = false
  }
  /** By default scopt calls sys.exit when --help is in options, this un-defeats doNotExitOnHelp */
  def exitOnHelp(): Unit = {
    parser.terminateOnExit = true
  }

  /** Show usage and exit */
  def showUsageAsError(): Unit = parser.showUsageAsError()

  /* Mechanism to collect the options of  */
  private var registeredLibraries = scala.collection.mutable.ArrayBuffer[RegisteredLibrary]()
  private def addRegisteredOptions(): Unit = {
    val iter = ServiceLoader.load(classOf[RegisteredLibrary]).iterator()
    while (iter.hasNext) {
      val lib = iter.next()
      registeredLibraries += lib
      parser.note(lib.name)
      lib.addOptions(parser)
    }
  }

  /* Mechanism to collect the options of individual transforms */
  private var registeredTransforms = scala.collection.mutable.ArrayBuffer[RegisteredTransform]()
  private def addTransformOptions(): Unit = {
    val iter = ServiceLoader.load(classOf[RegisteredTransform]).iterator()
    parser.note("FIRRTL Transform Options")
    while (iter.hasNext) {
      val tx = iter.next()
      registeredTransforms += tx
      tx.addOptions(parser)
    }
  }

  /** The [[AnnotationSeq]] generated from command line arguments
    *
    * This requires lazy evaluation as subclasses will mixin new command
    * line options via methods of [[ExecutionOptionsManager.parser]]
    */
  lazy implicit final val options: AnnotationSeq = {
    addTransformOptions
    addRegisteredOptions
    parser
      .parse(args, annotations)
      .getOrElse(throw new FIRRTLException("Failed to parse command line options"))
  }

  /* A hidden option that exposes what libraries and transforms are
   * registered. This is purely for debugging. */
  parser.opt[Unit]("show-registrations")
    .hidden()
    .action{ (_, c) =>
      println(s"""|Registered Libraries:
                  |  - ${registeredLibraries.map(_.getClass.getName).mkString("\n  - ")}
                  |Registered Transforms:
                  |  - ${registeredTransforms.map(_.getClass.getName).mkString("\n  - ")}""".stripMargin)
      c
    }

}
