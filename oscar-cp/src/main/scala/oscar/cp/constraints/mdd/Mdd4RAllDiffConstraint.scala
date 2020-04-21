package oscar.cp.constraints.mdd

import oscar.cp.core.variables.CPIntVar

object Mdd4RAllDiffConstraint {
  def apply(variable: Array[CPIntVar], predecessor: Seq[(Int, Int)]): Mdd4RConstraint = {
    val Arity: Int = variable.length
    val mdd = new StaticMddImpl(Arity)
    var curNode = mdd.root
    var i = 0
    while (i < Arity) {
      var dest: StaticMddNode = null
      if (i < Arity - 1) {
        dest = new StaticMddNode(i + 1)
        mdd.layers(i + 1).add(dest)
      }
      else {
        dest = mdd.end
      }
      for (value <- variable(i)) {
        val edge = new StaticMddEdge(curNode, dest, value)
      }
      i += 1
      curNode = dest
    }
    var mapNodeToAllDownState = scala.collection.mutable.Map[StaticMddNode, Set[Int]](mdd.root -> Set.empty)
    var mapNodeToAllUpState = scala.collection.mutable.Map[StaticMddNode, Set[Int]](mdd.end -> Set.empty)

    val AllDiffConstraint = new StaticMddConstraint {
      override def computeDownStates(node: StaticMddNode): Unit = {
        var intersectionSet: Set[Int] = Set.empty
        val inEdgeIterator = node.getInEdgeIterator()
        var starts = false
        while(inEdgeIterator.hasNext){
          val inEdge = inEdgeIterator.next()
          if(intersectionSet.isEmpty && !starts){
            intersectionSet = mapNodeToAllDownState(inEdge.topNode).union(Set(inEdge.value))
            starts = true
          }else{
            intersectionSet = mapNodeToAllDownState(inEdge.topNode).union(Set(inEdge.value)).intersect(intersectionSet)
          }
        }
        mapNodeToAllDownState += (node -> intersectionSet)
      }
      override def computeUpStates(node: StaticMddNode): Unit = {
        var intersectionSet: Set[Int] = Set.empty
        val outEdgeIterator = node.getOutEdgeIterator()
        var starts = false
        while(outEdgeIterator.hasNext) {
          val outEdge = outEdgeIterator.next()
          if (intersectionSet.isEmpty && !starts) {
            intersectionSet = mapNodeToAllUpState(outEdge.bottomNode).union(Set(outEdge.value))
            starts = true
          } else {
            intersectionSet = mapNodeToAllUpState(outEdge.bottomNode).union(Set(outEdge.value)).intersect(intersectionSet)
          }
        }
        mapNodeToAllUpState += (node -> intersectionSet)
      }

      override def shouldDeleteEdge(edge: StaticMddEdge): Boolean = {
        val topNode = edge.topNode
        val bottomNode = edge.bottomNode
        if(mapNodeToAllDownState(topNode).contains(edge.value))
          return true
        if(mapNodeToAllUpState(bottomNode).contains(edge.value))
          return true
        false
      }
      override def pruneSplittedNode(splittedNode: StaticMddSplittedNode): Unit = {
        val outEdgeIter = splittedNode.originalNode.getOutEdgeIterator()
        while(outEdgeIter.hasNext){
          val outEdge = outEdgeIter.next()
          if(shouldDeleteEdge(outEdge)){
            splittedNode.originalNode.removeOutEdge(outEdge.value)
          }
        }
      }
      override def clearStates(): Unit = {
        mapNodeToAllDownState.clear()
        mapNodeToAllUpState.clear()
      }
      override def clearStates(node: GlobalNode): Unit = {
        mapNodeToAllDownState(node.asInstanceOf[StaticMddNode]).empty
        mapNodeToAllUpState(node.asInstanceOf[StaticMddNode]).empty
      }
    }

    var mapNodeToSomeDownState = scala.collection.mutable.Map[StaticMddNode, Set[Int]](mdd.root -> Set.empty)
    var mapNodeToSomeUpState = scala.collection.mutable.Map[StaticMddNode, Set[Int]](mdd.root -> Set.empty)
    var mapVariableToListPredecessor = scala.collection.mutable.Map[Int, Set[Int]]()
    for(j <- 0 until Arity)
      mapVariableToListPredecessor(j) = Set.empty
    for(pred <- predecessor){
      mapVariableToListPredecessor += (pred._2 -> mapVariableToListPredecessor(pred._2).union(Set(pred._1)))
    }

    val SomeDiffConstraint = new StaticMddConstraint {
      override def computeDownStates(node: StaticMddNode): Unit = {
        var unionSet: Set[Int] = Set.empty
        val inEdgeIterator = node.getInEdgeIterator()
        while(inEdgeIterator.hasNext){
          val inEdge = inEdgeIterator.next()
          var starts = false
          if(unionSet.isEmpty && !starts){
            unionSet = mapNodeToSomeDownState(inEdge.topNode).union(Set(inEdge.value))
            starts = true
          }else{
            unionSet = mapNodeToSomeDownState(inEdge.topNode).union(Set(inEdge.value)).union(unionSet)
          }
        }
        mapNodeToSomeDownState += (node -> unionSet)
        //println("down state : " + mapNodeToSomeDownState(node))
      }
      override def computeUpStates(node: StaticMddNode): Unit = {
        var unionSet: Set[Int] = Set.empty
        val outEdgeIterator = node.getOutEdgeIterator()
        while(outEdgeIterator.hasNext){
          val outEdge = outEdgeIterator.next()
          var starts = false
          if(unionSet.isEmpty && !starts){
            unionSet = mapNodeToSomeUpState(outEdge.bottomNode).union(Set(outEdge.value))
            starts = true
          }else{
            unionSet = mapNodeToSomeUpState(outEdge.bottomNode).union(Set(outEdge.value)).union(unionSet)
          }
        }
        mapNodeToSomeUpState += (node -> unionSet)
        //println("up state : " + mapNodeToSomeUpState(node))
      }
      override def shouldDeleteEdge(edge: StaticMddEdge): Boolean = {
        val topNode = edge.topNode
        val bottomNode = edge.bottomNode
        if(mapNodeToSomeDownState(topNode).size == topNode.layer - mdd.root.layer && mapNodeToSomeDownState(topNode).contains(edge.value))
          return true
        if(mapNodeToSomeUpState(bottomNode).size == mdd.end.layer - bottomNode.layer && mapNodeToSomeUpState(bottomNode).contains(edge.value))
          return true
        if(mapNodeToSomeDownState(topNode).union(Set(edge.value)).union(mapNodeToSomeUpState(bottomNode)).size < Arity)
          return true
        if((0 until Arity).toSet.diff(mapNodeToSomeDownState(topNode)).exists(j => mapVariableToListPredecessor(edge.value).contains(j)))
          return true
        if((0 until Arity).toSet.diff(mapNodeToSomeUpState(bottomNode)).exists(j => mapVariableToListPredecessor(j).contains(edge.value)))
          return true
        false
      }
      override def pruneSplittedNode(splittedNode: StaticMddSplittedNode): Unit = {
        val outEdgeIter = splittedNode.originalNode.getOutEdgeIterator()
        while(outEdgeIter.hasNext){
          val outEdge = outEdgeIter.next()
          if(shouldDeleteEdge(outEdge)){
            splittedNode.originalNode.removeOutEdge(outEdge.value)
          }
        }
      }
      override def clearStates(): Unit = {
        mapNodeToSomeDownState.clear()
        mapNodeToSomeUpState.clear()
      }
      override def clearStates(node: GlobalNode): Unit = {
        mapNodeToSomeDownState(node.asInstanceOf[StaticMddNode]).empty
        mapNodeToSomeUpState(node.asInstanceOf[StaticMddNode]).empty
      }
    }
    println("nodes: " + mdd.getNumberOfNodes() + " edges: "+ mdd.getNumberOfEdges() + " paths: " + mdd.numPaths())
    mdd.addConstraint(AllDiffConstraint)
    mdd.addConstraint(SomeDiffConstraint)
    mdd.refine(2)
    println("nodes: " + mdd.getNumberOfNodes() + " edges: "+ mdd.getNumberOfEdges() + " paths: " + mdd.numPaths())
    new Mdd4RConstraint(variable, mdd, variable(0).store)
  }

}
