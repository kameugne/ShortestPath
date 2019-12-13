package oscar

import scala.annotation.compileTimeOnly

/**
 * Created by dervalguillaume on 2/12/16.
 */
package object algo {
  var debug = false

  /**
   * In Scala 2.13, `scala.Seq` changed from aliasing `scala.collection.Seq` to aliasing
   * `scala.collection.immutable.Seq`.  In this code base usage of unqualified `Seq` is banned: use
   * `immutable.Seq` or `collection.Seq` instead.
   *
   * import scala.collection
   * import scala.collection.immutable
   *
   * This `Seq` trait is a dummy type to prevent the use of `Seq`.
   */
  @compileTimeOnly("Use immutable.Seq or collection.Seq")
  type Seq[+A1] = collection.Seq[A1]
  //private[cp] trait Seq[A1] extends collection.Seq[A1]

  /**
   * In Scala 2.13, `scala.IndexedSeq` changed from aliasing `scala.collection.IndexedSeq` to aliasing
   * `scala.collection.immutable.IndexedSeq`.  In this code base usage of unqualified `IndexedSeq` is
   * banned: use `immutable.IndexedSeq` or `collection.IndexedSeq`.
   *
   * import scala.collection
   * import scala.collection.immutable
   *
   * This `IndexedSeq` trait is a dummy type to prevent the use of `IndexedSeq`.
   */
  @compileTimeOnly("Use immutable.IndexedSeq or collection.IndexedSeq")
  type IndexedSeq[+A1] = collection.IndexedSeq[A1]
  //private[cp] trait IndexedSeq[A1] extends collection.IndexedSeq[A1]
}
