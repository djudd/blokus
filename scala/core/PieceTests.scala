package blokus

import org.junit.Test
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.assertFalse

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 5:28:27 PM
 * To change this template play File | Settings | File Templates.
 */

class PieceTests {

  @Test
  def One_hasPlacement_Origin {
    assertTrue(One() hasPlacement Seq((0,0)))
  }

  @Test
  def One_placementCount {
    assertEquals(1, One().placements.size)
  }

  @Test
  def One_not_hasPlacement_wrongSize {
    assertFalse(One() hasPlacement Seq((0,0), (1,0)))
  }

  @Test
  def One_not_hasPlacement_offOrigin {
    assertFalse(One() hasPlacement Seq((1,1)))
  }

  @Test
  def Two_hasPlacement_Horizontal {
    assertTrue(Two() hasPlacement Seq((0,0), (1,0)))
  }

  @Test
  def Two_hasPlacement_Vertical {
    assertTrue(Two() hasPlacement Seq((0,1), (0,0)))
  }

  @Test
  def Two_not_hasPlacement_Diagonal {
    assertFalse(Two() hasPlacement Seq((0,0), (1,1)))
  }

  @Test
  def Two_placementCount {
    assertEquals(2*2, Two().placements.size)
  }

  @Test
  def Three_hasPlacement_Horizontal {
    assertTrue(Three() hasPlacement Seq((0,0), (1,0), (2,0)))
  }

  @Test
  def Three_hasPlacement_Vertical {
    assertTrue(Three() hasPlacement Seq((0,0), (0,1), (0,2)))
  }

  @Test
  def Three_placementCount {
    assertEquals(3*2, Three().placements.size)
  }

  @Test
  def ShortI_hasPlacement_Horizontal {
    assertTrue(ShortI() hasPlacement Seq((0,0), (1,0), (2,0), (3,0)))
  }

  @Test
  def ShortI_hasPlacement_Vertical {
    assertTrue(ShortI() hasPlacement Seq((0,0), (0,1), (0,2), (0,3)))
  }

  @Test
  def ShortI_placementCount {
    assertEquals(4*2, ShortI().placements.size)
  }

  @Test
  def LongI_hasPlacement_Horizontal {
    assertTrue(LongI() hasPlacement Seq((0,0), (1,0), (2,0), (3,0), (4,0)))
  }

  @Test
  def LongI_hasPlacement_Vertical {
    assertTrue(LongI() hasPlacement Seq((0,0), (0,1), (0,2), (0,3), (0,4)))
  }

  @Test
  def LongI_placementCount {
    assertEquals(5*2, LongI().placements.size)
  }

  @Test
  def X_hasPlacement_Centered {
    assertTrue(X() hasPlacement Seq((0,0), (1,0), (0,1), (-1,0), (0,-1)))
  }

  @Test
  def X_placementCount {
    assertEquals(5, X().placements.size)
  }

  @Test
  def Square_hasPlacement {
    assertTrue(Square() hasPlacement Seq((0,0), (1,0), (0,1), (1,1)))
  }

  @Test
  def Square_placementCount {
    assertEquals(4, Square().placements.size)
  }

  @Test
  def CrookedThree_hasPlacement_RotatedZero {
    assertTrue(CrookedThree() hasPlacement Seq((0,0), (1,0), (0,1)))
  }

  @Test
  def CrookedThree_hasPlacement_RotatedNinety {
    assertTrue(CrookedThree() hasPlacement Seq((0,0), (0,1), (-1,0)))
  }

  @Test
  def CrookedThree_hasPlacement_RotatedOneEighty {
    assertTrue(CrookedThree() hasPlacement Seq((0,0), (-1,0), (0,-1)))
  }

  @Test
  def CrookedThree_hasPlacement_TwoSeventy {
    assertTrue(CrookedThree() hasPlacement Seq((0,0), (1,0), (0,-1)))
  }

  @Test
  def CrookedThree_placementCount {
    assertEquals(3*4, CrookedThree().placements.size)
  }

  @Test
  def ShortL_hasPlacement_Initial {
    assertTrue(ShortL() hasPlacement Seq((0,0), (1,0), (0,1), (0,2)))
  }

  @Test
  def ShortL_hasPlacement_FlippedVertical {
    assertTrue(ShortL() hasPlacement Seq((0,0), (-1,0), (0,1), (0,2)))
  }

  @Test
  def ShortL_hasPlacement_FlippedHorizontal {
    assertTrue(ShortL() hasPlacement Seq((0,0), (1,0), (0,-1), (0,-2)))
  }

  @Test
  def ShortL_hasPlacement_RotatedNinety_FlippedVertical {
    assertTrue(ShortL() hasPlacement Seq((0,0), (1,0), (0,1), (2,0)))
  }

  @Test
  def ShortL_placementCount {
    assertEquals(4*4*2, ShortL().placements.size)
  }

  @Test
  def LongL_hasPlacement_Initial {
    assertTrue(LongL() hasPlacement Seq((0,0), (1,0), (0,1), (0,2), (0,3)))
  }

  @Test
  def LongL_hasPlacement_RotatedOneEighty_FlippedHorizontal {
    assertTrue(LongL() hasPlacement Seq((0,0), (-1,0), (0,1), (0,2), (0,3)))
  }

  @Test
  def LongL_placementCount {
    assertEquals(5*4*2, LongL().placements.size)
  }

  @Test
  def P_hasPlacement_Initial {
    assertTrue(P() hasPlacement Seq((0,0), (1,0), (1,1), (0,1), (0,2)))
  }

  @Test
  def P_hasPlacement_RotatedTwoSeventy_FlippedHorizontal {
    assertTrue(P() hasPlacement Seq((0,0), (1,0), (0,1), (1,1), (2,0)))
  }

  @Test
  def P_placementCount {
    assertEquals(5*4*2, P().placements.size)
  }

  @Test
  def ShortT_hasPlacement {
    assertTrue(ShortT() hasPlacement Seq((0,0), (1,0), (-1,0), (0,1)))
  }

  @Test
  def ShortT_placementCount {
    assertEquals(4*4, ShortT().placements.size)
  }

  @Test
  def LongT_hasPlacement {
    assertTrue(LongT() hasPlacement Seq((0,0), (1,0), (-1,0), (0,1), (0,2)))
  }

  @Test
  def LongT_placementCount {
    assertEquals(5*4, LongT().placements.size)
  }

  @Test
  def ShortZ_hasPlacement {
    assertTrue(ShortZ() hasPlacement Seq((0,0), (1,0), (1,1), (2,1)))
  }

  @Test
  def ShortZ_placementCount {
    assertEquals(4*4, ShortZ().placements.size)
  }

  @Test
  def LongZ_hasPlacement {
    assertTrue(LongZ() hasPlacement Seq((0,0), (0,1), (-1,1), (0,-1), (1,-1)))
  }

  @Test
  def LongZ_placementCount {
    assertEquals(5*4, LongZ().placements.size)
  }

  @Test
  def N_hasPlacement {
    assertTrue(N() hasPlacement Seq((0,0), (1,0), (1,1), (2,1), (3,1)))
  }

  @Test
  def N_placementCount {
    assertEquals(5*4*2, N().placements.size)
  }

  @Test
  def Y_hasPlacement {
    assertTrue(Y() hasPlacement Seq((0,0), (0,1), (0,2), (0,-1), (1,0)))
  }

  @Test
  def Y_placementCount {
    assertEquals(5*4*2, Y().placements.size)
  }

  @Test
  def F_hasPlacement {
    assertTrue(F() hasPlacement Seq((0,0), (-1,0), (0,1), (0,-1), (1,-1)))
  }

  @Test
  def F_placementCount {
    assertEquals(5*4*2, F().placements.size)
  }

  @Test
  def V_hasPlacement {
    assertTrue(V() hasPlacement Seq((0,0), (1,0), (2,0), (0,1), (0,2)))
  }

  @Test
  def V_placementCount {
    assertEquals(5*4, V().placements.size)
  }

  @Test
  def W_hasPlacement {
    assertTrue(W() hasPlacement Seq((0,0), (-1,0), (-1,-1), (0,1), (1,1)))
  }

  @Test
  def W_placementCount {
    assertEquals(5*4, W().placements.size)
  }

  @Test
  def U_hasPlacement {
    assertTrue(U() hasPlacement Seq((0,0), (-1,0), (-1,1), (1,0), (1,1)))
  }

  @Test
  def U_placementCount {
    assertEquals(5*4, U().placements.size)
  }

}