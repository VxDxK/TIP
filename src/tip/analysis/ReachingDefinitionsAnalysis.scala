package tip.analysis

import tip.ast._
import tip.ast.AstNodeData._
import tip.cfg._
import tip.lattices.{MapLattice, PowersetLattice}
import tip.solvers.{SimpleMapLatticeFixpointSolver, SimpleWorklistFixpointSolver}
import tip.ast.AstNodeData.DeclarationData

import scala.collection.immutable.Set

case class DefSite(vardecl: ADeclaration, node: CfgNode)

abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {

  val allDefs: Set[DefSite] = cfg.nodes.flatMap {
    case r: CfgStmtNode =>
      r.data match {
        case as: AAssignStmt =>
          as.left match {
            case id: AIdentifier => Some(DefSite(id.declaration, r))
            case _ => None
          }
        case _ => None
      }
    case _ => None
  }

  val lattice: MapLattice[CfgNode, PowersetLattice[DefSite]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunEntryNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              case id: AIdentifier =>
                val gen = Set(DefSite(id.declaration, r))
                val kill = allDefs.filter(_.vardecl == id.declaration)
                gen ++ (s -- kill)
              case _ => ???
            }
          case _ => s
        }
      case _ => s
    }
}

class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
