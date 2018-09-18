// See LICENSE for license details

package firrtl

import firrtl.ir.Circuit
import firrtl.annotations.NoTargetAnnotation
import firrtl.transforms.BlackBoxTargetDirAnno
import firrtl.options.HasScoptOptions
import logger.LogLevel
import scopt.OptionParser

/** Indicates that a subclass is an [[annotations.Annotation]] with an
  * option consummable by [[HasFirrtlExecutionOptions]]
  *
  * This must be mixed into a subclass of [[annotations.Annotation]]
  */
sealed trait FirrtlOption extends HasScoptOptions

/** Holds the name of the top module
  *  - maps to [[FirrtlExecutionOptions.topName]]
  *  - set on the command line with `-tn/--top-name`
  * @param value top module name
  */
case class TopNameAnnotation(topName: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("top-name")
    .abbr("tn")
    .valueName("<top-level-circuit-name>")
    .action( (x, c) => c :+ TopNameAnnotation(x) )
    .unbounded() // See [Note 1]
    .text("This options defines the top level circuit, defaults to dut when possible")
}

object TopNameAnnotation {
  private [firrtl] def apply(): TopNameAnnotation = TopNameAnnotation(topName = "")
}

/** Holds the name of the target directory
  *  - maps to [[FirrtlExecutionOptions.targetDirName]]
  *  - set with `-td/--target-dir`
  *  - if unset, a [[TargetDirAnnotation]] will be generated with the
  * @param value target directory name
  */
case class TargetDirAnnotation(targetDirName: String = ".") extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("target-dir")
    .abbr("td")
    .valueName("<target-directory>")
    .action( (x, c) => c ++ Seq(TargetDirAnnotation(x)) )
    .unbounded() // See [Note 1]
    .text(s"Work directory for intermediate files/blackboxes, default is ${FirrtlExecutionOptions().targetDirName}")
}

/** Additional arguments
  *  - maps to [[FirrtlExecutionOptions.programArgs]]
  *  - set with any trailing option on the command line
  * @param value one [[scala.String]] argument
  */
case class ProgramArgsAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.arg[String]("<arg>...")
    .unbounded()
    .optional()
    .action( (x, c) => c :+ ProgramArgsAnnotation(x) )
    .text("optional unbounded args")
}

object ProgramArgsAnnotation {
  private [firrtl] def apply(): ProgramArgsAnnotation = ProgramArgsAnnotation("")
}

/** An explicit input FIRRTL file to read
  *  - maps to [[FirrtlExecutionOptions.inputFileNameOverride]]
  *  - set with `-i/--input-file`
  *  - If unset, an [[InputFileAnnotation]] with the default input file __will not be generated__
  * @param value input filename
  */
case class InputFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("input-file")
    .abbr("i")
    .valueName ("<firrtl-source>")
    .action( (x, c) => c :+ InputFileAnnotation(x) )
    .unbounded() // See [Note 1]
    .text("use this to override the default input file name, default is empty")
}

object InputFileAnnotation {
  private [firrtl] def apply(): InputFileAnnotation = InputFileAnnotation("")
}

/** An explicit output file the emitter will write to
  *   - maps to [[FirrtlExecutionOptions.outputFileNameOverride]]
  *   - set with `-o/--output-file`
  *  @param value output filename
  */
case class OutputFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("output-file")
    .abbr("o")
    .valueName("<output>")
    .action( (x, c) => c :+ OutputFileAnnotation(x) )
    .unbounded()
    .text("use this to override the default output file name, default is empty")
}

object OutputFileAnnotation {
  private [firrtl] def apply(): OutputFileAnnotation = OutputFileAnnotation("")
}

/** An explicit output _annotation_ file to write to
  *  - maps to [[FirrtlExecutionOptions.outputAnnotationFileName]]
  *  - set with `-foaf/--output-annotation-file`
  * @param value output annotation filename
  */
case class OutputAnnotationFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("output-annotation-file")
    .abbr("foaf")
    .valueName ("<output-anno-file>")
    .action( (x, c) => c :+ OutputAnnotationFileAnnotation(x) )
    .unbounded() // See [Note 1]
    .text("use this to set the annotation output file")
}

object OutputAnnotationFileAnnotation {
  private [firrtl] def apply(): OutputAnnotationFileAnnotation = OutputAnnotationFileAnnotation("")
}

/** Sets the info mode style
  *  - maps to [[FirrtlExecutionOptions.infoModeName]]
  *  - set with `--info-mode`
  * @param value info mode name
  */
case class InfoModeAnnotation(value: String = "append") extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("info-mode")
    .valueName ("<ignore|use|gen|append>")
    .action( (x, c) => c :+ InfoModeAnnotation(x.toLowerCase) )
    .unbounded() // See [Note 1]
    .text(s"specifies the source info handling, default is ${FirrtlExecutionOptions().infoModeName}")
}

/** Holds a [[scala.String]] containing FIRRTL source to read as input
  *  - maps to [[FirrtlExecutionOptions.firrtlSource]]
  *  - set with `--firrtl-source`
  * @param value FIRRTL source as a [[scala.String]]
  */
case class FirrtlSourceAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("firrtl-source")
    .valueName ("A FIRRTL string")
    .action( (x, c) => c :+ FirrtlSourceAnnotation(x) )
    .unbounded() // See [Note 1]
    .text(s"A FIRRTL circuit as a string")
}

object FirrtlSourceAnnotation {
  private [firrtl] def apply(): FirrtlSourceAnnotation = FirrtlSourceAnnotation("")
}

/**  Indicates that an emitted circuit (FIRRTL, Verilog, etc.) will be one file per module
  *   - maps to [[FirrtlExecutionOptions.emitOneFilePerModule]]
  *   - set with `--split-modules`
  */
case object EmitOneFilePerModuleAnnotation extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("split-modules")
    .abbr("fsm")
    .action( (x, c) => c :+ EmitOneFilePerModuleAnnotation )
    .unbounded()
    .text ("Emit each module to its own file in the target directory.")
}

/** Holds a filename containing one or more [[annotations.Annotation]] to be read
  *  - this is not stored in [[FirrtlExecutionOptions]]
  *  - set with `-faf/--annotation-file`
  * @param value input annotation filename
  */
case class InputAnnotationFileAnnotation(value: String) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("annotation-file")
    .abbr("faf")
    .unbounded()
    .valueName("<input-anno-file>")
    .action( (x, c) => c :+ InputAnnotationFileAnnotation(x) )
    .text("Used to specify annotation file")
}

object InputAnnotationFileAnnotation {
  private [firrtl] def apply(): InputAnnotationFileAnnotation = InputAnnotationFileAnnotation("")
}

/** Holds the name of the compiler to run
  *  - maps to [[FirrtlExecutionOptions.compilerName]]
  *  - set with `-X/--compiler`
  *  - If unset, a [[CompilerNameAnnotation]] with the default compiler ("verilog") __will be generated__
  * @param value compiler name
  */
case class CompilerNameAnnotation(value: String = "verilog") extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("compiler")
    .abbr("X")
    .valueName ("<high|middle|low|verilog|sverilog>")
    .action( (x, c) => c :+ CompilerNameAnnotation(x) )
    .unbounded() // See [Note 1]
    .text(s"compiler to use, default is 'verilog'")
}

/** Holds the unambiguous class name of a [[Transform]] to run
  *  - will be append to [[FirrtlExecutionOptions.customTransforms]]
  *  - set with `-fct/--custom-transforms`
  * @param value the full class name of the transform
  */
case class RunFirrtlTransformAnnotation(transform: Class[_ <: Transform]) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Seq[String]]("custom-transforms")
    .abbr("fct")
    .valueName ("<package>.<class>")
    .validate( x => {
                x.map(txName =>
                  try { Class.forName(txName).asInstanceOf[Class[_ <: Transform]].newInstance() }
                  catch {
                    case e: ClassNotFoundException => throw new FIRRTLException(
                      s"Unable to locate custom transform $txName (did you misspell it?)", e)
                    case e: InstantiationException => throw new FIRRTLException(
                      s"Unable to create instance of Transform $txName (is this an anonymous class?)", e)
                    case e: Throwable => throw new FIRRTLException(
                      s"Unknown error when instantiating class $txName", e) } )
                p.success } )
    .action( (x, c) => c ++ x.map(txName =>
              RunFirrtlTransformAnnotation(Class.forName(txName).asInstanceOf[Class[_ <: Transform]])) )
    .unbounded()
    .text("runs these custom transforms during compilation.")
}

object RunFirrtlTransformAnnotation {
  private [firrtl] def apply(): RunFirrtlTransformAnnotation = RunFirrtlTransformAnnotation(classOf[Transform])
}

/** Holds a FIRRTL [[Circuit]]
  * @param value a circuit
  */
case class FirrtlCircuitAnnotation(value: Circuit) extends NoTargetAnnotation with FirrtlOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = Unit
}
