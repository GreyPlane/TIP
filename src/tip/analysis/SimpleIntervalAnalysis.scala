package tip.analysis

import tip.ast.ADeclaration
import tip.ast.AstNodeData.DeclarationData
import tip.cfg.{CfgNode, IntraproceduralProgramCfg}
import tip.lattices.{IntervalLattice, MapLattice, SignLattice}

class SimpleIntervalAnalysis(val cfg: IntraproceduralProgramCfg)(implicit val declData: DeclarationData)
    extends FlowSensitiveAnalysis(false)
    with ValueAnalysisMisc {

  val valuelattice: IntervalLattice.type = IntervalLattice
  val stateLattice: MapLattice[ADeclaration, IntervalLattice.type] = new MapLattice(IntervalLattice)
  val lattice: MapLattice[CfgNode, stateLattice.type] = new MapLattice(stateLattice)

  def analyze(): lattice.Element = ???
}
