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
import Prop.{forAll, someFailing, noneFailing, sizedProp, classify, propBoolean}
import Arbitrary._
import Shrink._
import Test.{Exhausted, Failed}

object ShrinkGuardSpecification extends Properties("ShrinkGuard") {

  val g1 = choose(1, 100)
  
  def propShouldFailWithShrinkedArg[T](prms: Test.Params, p: Prop, argCond: T => Boolean) = 
    Test.check(prms.copy(minSuccessfulTests = 1000), p).status match {
      case Failed(Arg(_,a,_,_)::Nil,_) if argCond(a.asInstanceOf[T]) => true
      case Exhausted => sys.error("test exhausted")
      case x => println("***"+x); false
    }
  
  property("ShrinkGuard.choose") = forAll { (prms: Test.Params, a: Int, b: Int) => 
    (a <= b) ==> {
      val g = choose(a, b)
      val k = ((a:Long) + b)/2
      val p = forAll(g)((n: Int) => n > k)
      propShouldFailWithShrinkedArg(prms, p, {(x: Int) => (a <= x && x <= k)})
    }
  }
  
  val nonExtremeRange = choose(-10000, 10000)
    
  property("ShrinkGuard.suchThat") = {
    implicit val arbInt = Arbitrary(nonExtremeRange)
    forAll { (prms: Test.Params, a: Int, b: Int) => (a + 2 <= b) ==> {
      val (ia, ib) = (a + 1, b - 1)
      val g = choose(a, b) suchThat { x => (ia <= x && x <= ib)}
      val k = (((ia:Long) + ib)/2).toInt
      val p = forAll(g){(n: Int) => n > k}
      propShouldFailWithShrinkedArg(prms, p, {(x: Int) => (ia <= x && x <= k)})
    }}
  }
  
  // The mapped function would need to be a bijection in order to
  // be able to check the generated value.
  // 
  // We can't decide if the fun is a bijection without help, so this doesn't work as expected.
  // Currently when being mapped, the resulting Gen will have no fence.
  val g4 = g1 suchThat { x => (1 <= x && x <= 100)} map { _ + 1 }
  property("[FAIL map] good shrink if ARG_0 = 2") = forAll(g4)((n: Int) => n > 3)

  // Bijections work
  val g5: Gen[Int] = (g1 suchThat { x => (1 <= x && x <= 100)}).bimap(_ + 1)(x => Some(x - 1))
  property("[bimap] good shrink if ARG_0 = 2") = forAll(g5)((n: Int) => n > 3)
  
  // Container sample
  val want = 3
  val cg1 = listOfN(want, g1)
  property("[listOfN] good shrink if no IOOBE and all three elem of ARG_0 is 1") = forAll(cg1) { xs: List[Int] =>
    xs(want - 1) > 3
  }
  
  // Multi-generator forAll
  val mg1 = choose(1, 10)
  val mg2 = choose(2, 10)
  property("[multi-gen forAll] good shrink if ARG_0 = (1, 2)") = forAll(mg1, mg2) { (x, y) => x > 3 && y > 3 }
  
  // Sized sample
  
}
