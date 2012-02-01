/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import Gen._
import Prop.{forAll, someFailing, noneFailing, sizedProp}
import Arbitrary._
import Shrink._

object ShrinkGuardSpecification extends Properties("ShrinkGuard") {

  property("alma") = forAll((n: Int) => n > -5 || n <= -4)
  
  val g1 = choose(1, 100)
  property("bad shrink ARG0=0") = forAll(g1)((n: Int) => n > 3)
  
  val g2 = g1 wc { x => (1 <= x && x <= 100)}
  property("good shrink ARG0=1") = forAll(g2)((n: Int) => n > 3)
  
  val g3 = g1 wc { x => (1 <= x && x <= 100)} suchThat { _ > 2}
  property("not-so-good shrink ARG0=0 or 1") = forAll(g3)((n: Int) => n > 3)
  
  override def main(args: Array[String]): Unit = {
    println("HEEELLLOOOOOO")
    /*
    val res = Test.checkProperties(Test.Params(minSuccessfulTests = 10), this)
    val failed = res.filter(!_._2.passed).size
    if (mainCallsExit)
      System exit failed
    */
  }
}
