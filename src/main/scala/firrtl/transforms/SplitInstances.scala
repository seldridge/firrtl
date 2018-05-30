// See LICENSE for license details.

package firrtl.transforms

import firrtl._
import firrtl.annotations.{ModuleName, SingleTargetAnnotation}
import firrtl.analyses.InstanceGraph

sealed case class SplitInstanceException(m: String) extends FIRRTLException(m)

case class SplitInstanceAnnotation(
  target: ModuleName,
  paths: Seq[Seq[WDefInstance]],
  newName: Option[String] = None,
  prefix: Option[String] = None) extends SingleTargetAnnotation[ModuleName] {
  paths.foreach{ p =>
    if (p.last.module != target.name) {
      throw new SplitInstanceException(
        s"Path leaf module (${p.last.serialize}) was not equal to target module (${target.serialize})") }
  }
  if(paths.map(_.size == 1).reduce(_|_)) {
    throw new SplitInstanceException("The top module cannot be split") }
  if (paths.map(_.head).toSet.size != 1) {
    throw new SplitInstanceException(
      s"""Paths have different top modules (${paths.map(_.head).distinct.map(x => x.module).mkString(", ")})""") }

  def main: String = paths.head.head.module

  def duplicate(t: ModuleName): SplitInstanceAnnotation = SplitInstanceAnnotation(t, paths, newName, prefix)
}

class SplitInstances extends Transform {
  def inputForm: CircuitForm = UnknownForm
  def outputForm: CircuitForm = UnknownForm

  def execute(state: CircuitState): CircuitState = {
    val statex = state.annotations
      .collect{ case a: SplitInstanceAnnotation => a }
      .foldLeft(state){ case (s, a) =>
        val iGraph = new InstanceGraph(state.circuit)
        if (state.circuit.main != a.main) { throw new SplitInstanceException("Path does not have circuit's top module") }
        val knownInstances = iGraph.findInstancesInHierarchy(a.target.name)
        a.paths.foreach(p => if (!knownInstances.contains(p)) { throw new SplitInstanceException("Path does not exist") })
        s
      }
    statex

    ???
  }
}
