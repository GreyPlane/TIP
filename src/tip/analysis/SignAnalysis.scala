package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.ast._
import tip.cfg.{CfgNode, CfgStmtNode, InterproceduralProgramCfg, IntraproceduralProgramCfg}
import tip.lattices.SignLattice

object SignAnalysis {

  object Intraprocedural {

    /**
      * Intraprocedural analysis that uses the simple fixpoint solver.
      */
    class SimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends IntraprocValueAnalysisSimpleSolver(cfg, SignLattice) {

      /**
        * The transfer function.
        */
      override def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = {
        NoPointers.assertContainsNode(n.data)
        NoCalls.assertContainsNode(n.data)
        NoRecords.assertContainsNode(n.data)
        n match {
          case r: CfgStmtNode =>
            r.data match {
              // var declarations
              case varr: AVarStmt =>
                varr.declIds.foldLeft(s) {
                  case (ss, id) =>
                    ss.updated(id, statelattice.sublattice.top)
                }

              // assignments
              case AAssignStmt(id: AIdentifier, right, _) => s.updated(id, eval(right, s))

              // all others: like no-ops
              case _ => s
            }
          case _ => s
        }
      }

      /**
        * The constraint function for which the least fixpoint is to be computed.
        *
        * @param x the input lattice element
        * @return the output lattice element
        */
      override def fun(x: lattice.Element): lattice.Element = domain.foldLeft(lattice.bottom)(
        (m, a) =>
          m + (a -> {
            funsub(a, x)
          })
      )
    }

    /**
      * Intraprocedural analysis that uses the worklist solver.
      */
    class WorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends IntraprocValueAnalysisWorklistSolver(cfg, SignLattice) {

      /**
        * The transfer function.
        */
      override def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element = {
        NoPointers.assertContainsNode(n.data)
        NoCalls.assertContainsNode(n.data)
        NoRecords.assertContainsNode(n.data)
        n match {
          case r: CfgStmtNode =>
            r.data match {
              // var declarations
              case varr: AVarStmt =>
                varr.declIds.foldLeft(s) {
                  case (ss, id) =>
                    ss.updated(id, statelattice.sublattice.top)
                }

              // assignments
              case AAssignStmt(id: AIdentifier, right, _) => s.updated(id, eval(right, s))

              // all others: like no-ops
              case _ => s
            }
          case _ => s
        }
      }
    }

    /**
      * Intraprocedural analysis that uses the worklist solver with reachability.
      */
    class WorklistSolverWithReachability(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, SignLattice)

    /**
      * Intraprocedural analysis that uses the worklist solver with reachability and propagation-style.
      */
    class WorklistSolverWithReachabilityAndPropagation(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachabilityAndPropagation(cfg, SignLattice)
  }

  object Interprocedural {

    /**
      * Interprocedural analysis that uses the worklist solver with reachability.
      */
    class WorklistSolverWithReachability(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData)
        extends InterprocValueAnalysisWorklistSolverWithReachability(cfg, SignLattice)

    /**
      * Interprocedural analysis that uses the worklist solver with reachability and propagation-style.
      */
    class WorklistSolverWithReachabilityAndPropagation(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData)
        extends InterprocValueAnalysisWorklistSolverWithReachabilityAndPropagation(cfg, SignLattice)

    /**
      * Interprocedural analysis that uses the worklist solver with reachability and propagation-style.
      * with call-string context sensitivity.
      */
    class CallString(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData) extends CallStringValueAnalysis(cfg, SignLattice)

    /**
      * Interprocedural analysis that uses the worklist solver with reachability and propagation-style.
      * with functional-approach context sensitivity.
      */
    class Functional(cfg: InterproceduralProgramCfg)(implicit declData: DeclarationData) extends FunctionalValueAnalysis(cfg, SignLattice)
  }

}
