package tip.analysis

import tip.ast.AstNodeData.DeclarationData
import tip.cfg.IntraproceduralProgramCfg
import tip.lattices.SizeLattice

object VariableSizeAnalysis {

  object Intraprocedural {

    class SimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisSimpleSolver(cfg, SizeLattice)

    class WorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolver(cfg, SizeLattice)
  }
}
