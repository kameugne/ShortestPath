package oscar.cbls.business.routing.neighborhood

/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

import oscar.cbls.algo.search.{HotRestart, IdenticalAggregator}
import oscar.cbls.business.routing.model.VRP
import oscar.cbls.core.search.{First, EasyNeighborhoodMultiLevel, LoopBehavior}




/**
 * base class for point insertion moves
 * @author renaud.delandtsheer@cetic.be
 */
abstract class InsertPoint(vrp: VRP,
                            neighborhoodName: String)
  extends EasyNeighborhoodMultiLevel[InsertPointMove](neighborhoodName){

  val v = vrp.v
  val seq = vrp.routes

  var insertAtPositionForInstantiation:Int = -1
  var insertedPointForInstantiation:Int = -2

  override def instantiateCurrentMove(newObj: Int) =
    InsertPointMove(insertedPointForInstantiation, insertAtPositionForInstantiation, newObj, this, vrp, neighborhoodNameToString)

  def doMove(insertedPoint: Int, insertAtPosition:Int): Unit = {
    seq.insertAtPosition(insertedPoint, insertAtPosition)
  }
}

case class InsertPointMove(insertedPoint: Int,
                            insertAtPosition: Int,
                            override val objAfter: Int,
                            override val neighborhood: InsertPoint,
                            vrp:VRP,
                            override val neighborhoodName: String = "InsertPointMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, vrp){

  //TODO
  override def impactedPoints: List[Int] = List(insertedPoint)

  override def commit(): Unit = {
    neighborhood.doMove(insertedPoint, insertAtPosition)
  }

  override def toString: String =
    neighborhoodName + ":InsertPoint(insertedPoint:" + insertedPoint +
      " insertAtPosition:" + insertAtPosition + objToString + ")"
}


/**
 * Inserts an unrouted point in a route. The size of the neighborhood is O(u*n).
 * where u is the numberof unrouted points, and n is the number of routed points
 * it can be cut down to u*k by using the relevant neighbors, and specifying k neighbors for each unrouted point
 * @param unroutedNodesToInsert the nodes that this neighborhood will try to insert SHOULD BE NOT ROUTED
 * @param relevantPredecessor a function that, for each unrouted node gives a list of routed node
 *                          such that it is relevant to insert the unrouted node after this routed node
 * @param vrp the routing problem
 * @param neighborhoodName the name of this neighborhood
 * @param selectNodeBehavior how should it iterate on nodes to insert?
 * @param selectInsertionPointBehavior how should it iterate on position for insertion?
 * @param hotRestart set to true fo a hot restart fearture on the node to insert
 * @param nodeSymmetryClass a function that input the ID of an unrouted node and returns a symmetry class;
 *                      ony one of the unrouted node in each class will be considered for insert
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param hotRestartOnNextSymmetryClass when you have symmetries among points to insert and hotRestart,
 *                                  this option will try to have the hotRestart starting
 *                                  at a different symmetry class than the last one.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
case class InsertPointUnroutedFirst(unroutedNodesToInsert: () => Iterable[Int],
                                     relevantPredecessor: () => Int => Iterable[Int],
                                     vrp: VRP,
                                     neighborhoodName: String = "InsertPointUnroutedFirst",
                                     hotRestart: Boolean = true,
                                     selectNodeBehavior:LoopBehavior = First(),
                                     selectInsertionPointBehavior:LoopBehavior = First(),
                                     nodeSymmetryClass:Option[Int => Int] = None,
                                     hotRestartOnNextSymmetryClass:Boolean = false)
  extends InsertPoint(vrp: VRP,neighborhoodName){

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val iterationSchemeOnZone =
      if (hotRestart) HotRestart(unroutedNodesToInsert(), startIndice)
      else unroutedNodesToInsert()

    val iterationScheme = nodeSymmetryClass match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, s)
    }

    val relevantNeighborsNow = relevantPredecessor()

    val (nodeToInsertIterator,notifyFound1) = selectNodeBehavior.toIterator(iterationScheme)

    while (nodeToInsertIterator.hasNext) {
      insertedPointForInstantiation = nodeToInsertIterator.next()
      assert(!vrp.isRouted(insertedPointForInstantiation),
        "The search zone should be restricted to unrouted nodes when inserting.")

      val (positionToInsertIterable,notifyFound2) =
        selectInsertionPointBehavior.toIterable(relevantNeighborsNow(insertedPointForInstantiation))

      for(pointWhereToInsertAfter <- positionToInsertIterable){

        seqValue.positionOfAnyOccurrence(pointWhereToInsertAfter) match{
          case None => //not routed?!
          case Some(position) =>
            insertAtPositionForInstantiation = position + 1

            doMove(insertedPointForInstantiation, insertAtPositionForInstantiation)

            if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
              notifyFound1()
              notifyFound2()
            }
        }
      }
    }

    seq.releaseTopCheckpoint()

    //hotRestart stuff for next call
    startIndice = if (hotRestartOnNextSymmetryClass) {
      if (nodeToInsertIterator.hasUnboundedNext())
        nodeToInsertIterator.unboundedNext()
      else iterationScheme.head
    } else{insertedPointForInstantiation + 1}

  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * OnePoint insert neighborhood htat primarily iterates over insertion point,s and then over poitns that can be iserted.
 * @param insertionPoints the positions where we can insert points, can be unrouted, in this case it is ignored (but time is wasted)
 * @param relevantSuccessorsToInsert the points to insert, given an insertion point
 * @param vrp the routing problem
 * @param neighborhoodName the name of the neighborhood
 * @param selectInsertionPointBehavior specifies how the insertion point should be selected
 * @param selectInsertedNodeBehavior specifies how the inserted point should be selected
 * @param hotRestart hot restart on the insertion point
 * @param insertedPointsSymetryClass a function that input the ID of an unrouted node and returns a symmetry class;
 *                      ony one of the unrouted node in each class will be considered for insert
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @author renaud.delandtsheer@cetic.be
 */
case class InsertPointRoutedFirst(insertionPoints:()=>Iterable[Int],
                                   relevantSuccessorsToInsert: () => Int => Iterable[Int],
                                   vrp: VRP,
                                   neighborhoodName: String = "InsertPointRoutedFirst",
                                   selectInsertionPointBehavior:LoopBehavior = First(),
                                   selectInsertedNodeBehavior:LoopBehavior = First(),
                                   hotRestart: Boolean = true,
                                   insertedPointsSymetryClass:Option[Int => Int] = None)
  extends InsertPoint(vrp: VRP,neighborhoodName) {

  //the indice to start with for the exploration
  var startIndice: Int = 0
  var pointWhereToInsertAfter = 0

  override def exploreNeighborhood(): Unit = {
    val seqValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToTopCheckpoint(seqValue)
      a
    }

    val unroutedNodesToInsertNow = relevantSuccessorsToInsert()

    val (iterationSchemeOnInsertionPointIterator,notifyFound1) =
      selectInsertionPointBehavior.toIterator(
        if (hotRestart) HotRestart(insertionPoints(), startIndice)
        else insertionPoints()
      )

    while (iterationSchemeOnInsertionPointIterator.hasNext) {

      pointWhereToInsertAfter = iterationSchemeOnInsertionPointIterator.next()

      seqValue.positionOfAnyOccurrence(pointWhereToInsertAfter) match{
        case None => //not routed?
        case Some(position) =>
          insertAtPositionForInstantiation = position + 1

          val (iteratorOnPointsToInsert,notifyFound2) = selectInsertedNodeBehavior.toIterator(insertedPointsSymetryClass match {
            case None => unroutedNodesToInsertNow(pointWhereToInsertAfter)
            case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(unroutedNodesToInsertNow(pointWhereToInsertAfter), s)
          })

          while (iteratorOnPointsToInsert.hasNext) {
            insertedPointForInstantiation = iteratorOnPointsToInsert.next()

            doMove(insertedPointForInstantiation, insertAtPositionForInstantiation)

            if (evaluateCurrentMoveObjTrueIfSomethingFound(evalObjAndRollBack())) {
              notifyFound1()
              notifyFound2()
            }
          }
      }
    }

    seq.releaseTopCheckpoint()
    //hot restart
    startIndice = pointWhereToInsertAfter + 1
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}