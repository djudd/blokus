package blokus

import org.junit.Test
import org.junit.Assert;

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 11:10:10 PM
 * To change this template play File | Settings | File Templates.
 */

class ShiftTests {

  @Test
  def toCoord() {
    val moves = Down() :: Right() :: Right() :: Nil
    val cell = Shift.toPoint(0,0,moves)
    Assert.assertEquals((2,1), cell)
  }

  @Test
  def toPlacement() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val cells = Shift.toCoords(0,0,placement)
    Assert.assertEquals((0,0) :: (0,1) :: (1,1) :: Nil, cells)
  }

  @Test
  def rotateZero() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val rotated = Shift.rotateClockwise(placement, Zero())
    assertPlacementsEqual(placement, rotated)
  }

  @Test
  def rotateNinety() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val rotated = Shift.rotateClockwise(placement, Ninety())
    assertPlacementsEqual(List(Nil, List(Left()), List(Left(), Down())), rotated)
  }

  @Test
  def rotateTwoSeventy() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val rotated = Shift.rotateClockwise(placement, TwoSeventy())
    assertPlacementsEqual(List(Nil, List(Right()), List(Right(), Up())), rotated)
  }

  @Test
  def flipAcrossVertical() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val flipped = Shift.flipAcross(placement, Vertical())
    assertPlacementsEqual(List(Nil, List(Down()), List(Down(), Left())), flipped)
  }

  @Test
  def flipAcrossHorizontal() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val flipped = Shift.flipAcross(placement, Horizontal())
    assertPlacementsEqual(List(Nil, List(Up()), List(Up(), Right())), flipped)
  }

  @Test
  def translateNowhere() {
    val placement = List(Nil, List(Down()), List(Right(), Down()))
    val translated = Shift.translate(placement, Nil)
    assertPlacementsEqual(placement, translated)
  }

  @Test
  def translateUp() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val translated = Shift.translate(placement, List(Up()))
    assertPlacementsEqual(List(List(Up()), Nil, List(Right())), translated)
  }

  @Test
  def translateUpLeft() {
    val placement = List(Nil, List(Down()), List(Down(), Right()))
    val translated = Shift.translate(placement, List(Up(), Left()))
    assertPlacementsEqual(List(List(Up(), Left()), List(Left()), Nil), translated)
  }

  @Test
  def allTranslationsOverlappingOrigin() {
    val placement = List(Nil, List(Up()))
    assertPlacementSetsEqual(
      Set(List(Nil, List(Up())), List(Nil, List(Down()))),
      Shift.allTranslationsOverlappingOrigin(placement)
    )
  }

  @Test
  def allRotations() {
    val placement = List(Nil, List(Up()))
    assertPlacementSetsEqual(
      Set(List(Nil, List(Up())), List(Nil, List(Right())), List(Nil, List(Down())), List(Nil, List(Left()))),
      Shift.allRotations(placement)
    )
  }

  @Test
  def allFlips() {
    val placement = List(Nil, List(Up()), List(Right()))
    assertPlacementSetsEqual(
      Set(
        List(Nil, List(Up()), List(Right())),
        List(Nil, List(Down()), List(Right())),
        List(Nil, List(Up()), List(Left()))
      ),
      Shift.allFlips(placement)
    )
  }

  @Test
  def allPlacements_One() {
    assertPlacementSetsEqual(Set(List(Nil)), Shift.allPlacements(List(Nil)))
  }

  @Test
  def allPlacements_Two() {
    val placement = List(Nil, List(Up()))
    assertPlacementSetsEqual(
      Set(
        List(Nil, List(Up())),
        List(Nil, List(Down())),
        List(Nil, List(Right())),
        List(Nil, List(Left()))
      ),
      Shift.allPlacements(placement)
    )
  }

  @Test
  def dedup_neighboringUpDown() {
    val path = List(Up(), Down(), Right())
    Assert.assertEquals(Set(Right()), Shift.dedup(path).toSet)
  }

  @Test
  def dedup_nonneighboringLeftRight() {
    val path = List(Left(), Up(), Right())
    Assert.assertEquals(Set(Up()), Shift.dedup(path).toSet)
  }

  @Test
  def dedup_noChanges() {
    val path = List(Up(), Right())
    Assert.assertEquals(path.toSet, Shift.dedup(path).toSet)
  }

  @Test
  def dedup_nil() {
    Assert.assertEquals(Nil, Shift.dedup(Nil))
  }

  private def assertPlacementSetsEqual(expected: Set[Shift.Placement], actual: Set[Shift.Placement]) = {
    val e = expected.map(placement => placement.map(path => path.toSet).toSet).toSet
    val a = actual.map(placement => placement.map(path => path.toSet).toSet).toSet
    Assert.assertEquals(e, a)
  }

  private def assertPlacementsEqual(expected: Shift.Placement, actual: Shift.Placement) = {
    val e = expected.map(path => path.toSet).toSet
    val a = actual.map(path => path.toSet).toSet
    Assert.assertEquals(e, a)
  }

}