/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  Added by Robin Palotai, 2012                                           **
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

  // choose for Int and Long is now equipped with fence (guard)
  val g1 = choose(1, 100)
  property("[choose] good shrink if ARG_0 = 1") = forAll(g1)((n: Int) => n > 3)
  
  // suchThat/filter adds extra guard over present ones
  val g2 = g1 suchThat { x => (1 <= x && x <= 100)}
  property("[suchThat] good shrink if ARG_0 = 1") = forAll(g2)((n: Int) => n > 3)
  
  val g3 = g1 suchThat { x => (1 <= x && x <= 100)} suchThat { _ > 2}
  property("[suchThat 2] good shrink if ARG_0 = 3") = forAll(g3)((n: Int) => n > 3)  
  
  // The mapped function would need to be a bijection in order to
  // be able to check the generated value.
  // 
  // We can't decide if the fun is a bijection without help, so this doesn't work as expected.
  // Currently when being mapped, the resulting Gen will have no fence.
  val g4 = g1 suchThat { x => (1 <= x && x <= 100)} map { _ + 1 }
  property("[FAIL map] good shrink if ARG_0 = 2") = forAll(g4)((n: Int) => n > 3)

  // Bijections work
  val g5 = g1 suchThat { x => (1 <= x && x <= 100)} bimap ({_ + 1}, (x: Int) => Some(x - 1))
  property("[bimap] good shrink if ARG_0 = 2") = forAll(g5)((n: Int) => n > 3)
  
  // Container sample
  val want = 3
  val cg1 = listOfN(want, g2)
  property("[listOfN] good shrink if no IOOBE and all three elem of ARG_0 is 1") = forAll(cg1) { xs: List[Int] =>
    xs(want - 1) > 3
  }
  
  // Multi-generator forAll
  val mg1 = choose(1, 10)
  val mg2 = choose(2, 10)
  property("[multi-gen forAll] good shrink if ARG_0 = (1, 2)") = forAll(mg1, mg2) { (x, y) => x > 3 && y > 3 }
}
