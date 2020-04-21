package oscar.cp.constraints.mdd

import java.util

import oscar.algo.reversible.{ReversibleContext, ReversibleSharedSparseSet}
import oscar.cp.Constraint
import oscar.cp.core.{CPPropagStrength, CPStore}
import oscar.cp.core.variables.{CPInsertSeqVar, CPVar}

abstract class Mdd4RSequenceConstraint(sequence: CPInsertSeqVar, mdd: StaticMdd, reversibleContext: ReversibleContext)
  extends Constraint(sequence.store, "MddGlobalSeq") {

  protected var mddWidth = 1

  def setMddWidth(width: Int): Unit = mddWidth = width

  override def associatedVars(): Iterable[CPVar] = Array(sequence)

  priorityL2 = CPStore.MinPriorityL2

  override def setup(l: CPPropagStrength): Unit = {
    //Setting up propagation strength:
    l match {
      case CPPropagStrength.Weak =>
        mddWidth = 1
      case CPPropagStrength.Medium =>
        mddWidth = 16
      case CPPropagStrength.Strong =>
        mddWidth = 64
      case _ =>
        mddWidth = 1024
    }



    propagate()

    sequence.callPropagateWhenDomainChanges(this)
  }
}
class Mdd4RSeqConstraint(sequence: CPInsertSeqVar, mdd: StaticMdd, reversibleContext: ReversibleContext)
  extends Mdd4RSequenceConstraint(sequence, mdd, reversibleContext){

  idempotent = true

  private[this] val nEdges = mdd.getNumberOfEdges()
  private[this] val nNodes = mdd.getNumberOfNodes()

  private[this] val nVariables = sequence.allRequired.size

  private[this] val staticDomains: Array[util.TreeSet[Int]] = mdd.staticDomains()

  private[this] val nodes: Array[ReversibleMddNode] = Array.ofDim(nNodes)
  private[this] val edges: Array[ReversibleMddEdge] = Array.ofDim(nEdges)
  private[this] val nodesMapping: util.HashMap[Long, Int] = mdd.mapNodes(nodes, reversibleContext)
  private[this] val edgesMapping: util.HashMap[Long, Int] = mdd.mapEdgesAndLink(edges, nodes, nodesMapping)
  private[this] val supports: Array[Array[ReversibleSharedSparseSet]] = setSupports()








  private def setSupports(): Array[Array[ReversibleSharedSparseSet]] = {
    //val outputSupportSet = Array.fill[util.HashMap[Int, ReversibleSharedSparseSet]](nVariables)(new util.HashMap[Int, ReversibleSharedSparseSet]())

    val nValues = if (nEdges == 0) 0 else staticDomains.map(_.last()).max + 1

    val outputSupportSet = Array.ofDim[ReversibleSharedSparseSet](nVariables,nValues)

    for (i <- 0 until nEdges) {
      val edge = edges(i)
      var sup = outputSupportSet(edge.variableId)(edge.value)
      if (sup == null) {
        sup = new ReversibleSharedSparseSet(reversibleContext, nEdges)
        outputSupportSet(edge.variableId)(edge.value) =  sup
      }
      sup.insert(edge.id)
    }
    outputSupportSet
  }
}
