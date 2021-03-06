package oscar.cp.searches.lns.operators

import oscar.algo.search.SearchStatistics
import oscar.cp.searches.lns.CPIntSol
import oscar.cp.searches.lns.selection.AdaptiveStore

import scala.xml.Elem

/**
  * Adaptive large neighbourhood search operator with two parameters of type T1 and T2.
  *
  * @param function the function that the operator applies.
  * @param param1Store: an AdaptiveStore containing the possible values for the 1st parameter.
  * @param param2Store: an AdaptiveStore containing the possible values for the 2nd parameter.
  */
class ALNSTwoParamsOperator[T1, T2](
                                     name: String,
                                     failThreshold: Int,
                                     val function: (T1, T2) => (CPIntSol => Unit, Option[Int], Option[Int]),
                                     val param1Store: AdaptiveStore[ALNSParameter[T1]],
                                     val param2Store: AdaptiveStore[ALNSParameter[T2]]
                                   ) extends ALNSOperator(name, failThreshold){

  private var selected1: Option[ALNSParameter[T1]] = None
  private var selected2: Option[ALNSParameter[T2]] = None

  /**
    * returns a reified operator representing this operator with the given parameter value.
    * @param param1 The index of the 1st parameter value in the internal adaptive store.
    * @param param2 The index of the 2nd parameter value in the internal adaptive store.
    */
  private def getReified(param1: ALNSParameter[T1], param2: ALNSParameter[T2]): ALNSReifiedOperator = {
    new ALNSReifiedOperator(
      name + "(" + param1.value + ", " + param2.value + ")",
      Math.min(param1.failThreshold, param2.failThreshold),
      () => function(param1.value, param2.value),
      (tStart, tEnd, objStart, objEnd, iterStats, fail, iter) =>
        updateParams(param1, param2, tStart, tEnd, objStart, objEnd, iterStats, fail, iter),
      (state) => {
        if(!param1.isActive) {
          param1Store.deactivate(param1)
          if (param1Store.isActiveEmpty) setActive(false)
        }
        if(!param2.isActive) {
          param2Store.deactivate(param2)
          if (param2Store.isActiveEmpty) setActive(false)
        }
      }
    )
  }

  def getReifiedParameters: Iterable[ALNSReifiedOperator] = for{
    p1 <- param1Store.getElements
    p2 <- param2Store.getElements
  } yield getReified(p1, p2)

  //Warning: risk if used concurrently!
  override def getFunction: (CPIntSol => Unit, Option[Int], Option[Int]) = {
    selected1 = Some(param1Store.select())
    selected2 = Some(param2Store.select())
    function(selected1.get.value, selected2.get.value)
  }

  override def update(
                       tStart: Long,
                       tEnd: Long,
                       objStart: Int,
                       objEnd: Int,
                       iterStats: SearchStatistics,
                       fail: Boolean,
                       iter: Long
                     ): Unit = {
    if(selected1.isDefined && selected2.isDefined){
      updateParams(selected1.get, selected2.get, tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
      selected1 = None
      selected2 = None
      super.update(tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
    }
    else throw new Exception("This operator has not been used!")
  }

  private def updateParams(
                            param1: ALNSParameter[T1],
                            param2: ALNSParameter[T2],
                            tStart: Long,
                            tEnd: Long,
                            objStart: Int,
                            objEnd: Int,
                            iterStats: SearchStatistics,
                            fail: Boolean,
                            iter: Long
                          ): Unit ={
    param1.update(tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
    param1Store.adapt(param1)
    if(!param1.isActive){
      param1Store.deactivate(param1)
      if(param1Store.isActiveEmpty) setActive(false)
      println("Operator " + name + " deactivated")
    }

    param2.update(tStart, tEnd, objStart, objEnd, iterStats, fail, iter)
    param2Store.adapt(param2)
    if(!param2.isActive){
      param2Store.deactivate(param2)
      if(param2Store.isActiveEmpty) setActive(false)
      println("Operator " + name + " deactivated")
    }
  }

  override def tuneParameters(): ALNSNoParamOperator = ???

  override def nParamVals: Int = nParamVals(1) * nParamVals(2)

  /**
    * Returns the number of active values for the specified parameter.
    */
  def nParamVals(param: Int): Int = param match{
    case 1 => param1Store.nActive
    case 2 => param2Store.nActive
  }

  override def setActive(state: Boolean): Unit = {
    if(state){
      param1Store.getElements.foreach(_.setActive(state))
      param2Store.getElements.foreach(_.setActive(state))
      param1Store.reset()
      param2Store.reset()
    }
    super.setActive(state)
  }

  override def resetFails(): Unit = {
    param1Store.getElements.foreach(_.resetFails())
    param2Store.getElements.foreach(_.resetFails())
    super.resetFails()
  }

  override def asXml(cat: String): Elem = {
    <operator>
      <name>{name}</name>
      <type>{cat}</type>
      {super.wrapStatsToXml()}
      <parameter1>{param1Store.getElements.map(_.asXml("parameter"))}</parameter1>
      <parameter2>{param2Store.getElements.map(_.asXml("parameter"))}</parameter2>
    </operator>
  }

  override def toString: String = {
    var s = super.toString
    s += "\n\tParameter 1:\n\t" + param1Store.getElements.mkString("\n\t")
    s += "\n\tParameter 2:\n\t" + param2Store.getElements.mkString("\n\t")
    s
  }
}
