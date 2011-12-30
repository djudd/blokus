package blokus

import core.{Diagonal, Straight}
import org.junit.{Test,Before}
import org.junit.Assert.{assertEquals, assertTrue, assertNotNull, fail}
import edu.uci.ics.jung.algorithms.shortestpath.UnweightedShortestPath


/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 3:41:04 PM
 * To change this template play File | Settings | File Templates.
 */

class BoardTests {

  var b:Board = null
  var p:Player = null

  @Before
  def setUp() {
    b = new Board()
    p = new Player(0,0)
  }

  @Test
  def canPlaceOneAtMyCorner() {
    b.place(p, Move(One(), (0,0), List((0,0))))
  }

  @Test
  def cannotPlaceOneInMiddleOfEmptyBoard() {
    try {
      b.place(p, Move(One(), (1,1), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceOneAtWrongCornerOfEmptyBoard() {
    try {
      b.place(p, Move(One(), (19,19), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def canPlaceOneInMiddleOfBoardTouchingCornerOfMyPiece() {
    b.place(p, Move(One(), (0,0), List((0,0))))
    b.place(p, Move(One(), (1,1), List((0,0))))
  }

  @Test
  def cannotPlaceOneInMiddleOfBoardTouchingCornerOfOtherPlayersPiece() {
    val other = new Player(19,19, id=2)
    b.place(other, Move(One(), (19,19), List((0,0))))

    try {
      b.place(p, Move(One(), (18,18), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def canPlaceOneTouchingSideOfOtherPlayersPiece() {
    val other = new Player(19,19, id=2)
    b.assign(other, List((1,0)))
    b.place(p, Move(One(), (0,0), List((0,0))))
  }

  @Test
  def cannotPlaceOneTouchingOnlySideOfMyPiece() {
    b.place(p, Move(One(), (0,0), List((0,0))))

    try {
      b.place(p, Move(One(), (0,1), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def canPlaceTwoTouchingCornerOfMyPiece() {
    p = new Player(19,19)
    b.place(p, Move(One(), (19,19), List((0,0))))
    b.place(p, Move(Two(), (18,18), List((0,0), (-1,0))))
  }

  @Test
  def cannotPlaceTwoTouchingBothSideAndCornerOfMyPiece() {
    b.place(p, Move(One(), (0,0), List((0,0))))

    try {
      b.place(p, Move(Two(), (0,1), List((0,0),(1,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceTwoTouchingBothSideAndCornerOfMyPiece_playerHasLeftmostIdBit() {
    p = new Player(0,0,id=4)

    b.place(p, Move(One(), (0,0), List((0,0))))

    try {
      b.place(p, Move(Two(), (0,1), List((0,0),(1,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceOneOffEmptyBoard() {
    try {
      b.place(p, Move(One(), (-1,-1), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceOneOffBoardEvenThoughTouchesCorner() {
    b.place(p, Move(One(), (0,0), List((0,0))))

    try {
      b.place(p, Move(One(), (-1,-1), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlacePartOfTwoOffBoard() {
    p = new Player(0,19)
    try {
      b.place(p, Move(Two(), (0,19), List((0,0),(0,1))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceOneOnTopOfOne() {
    p = new Player(0,0)
    b.place(p, Move(One(), (0,0), List((0,0))))
    try {
      b.place(p, Move(One(), (0,0), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceTwoOnTopOfOne() {
    p = new Player(0,19)
    b.place(p, Move(One(), (0,19), List((0,0))))
    try {
      b.place(p, Move(Two(), (0,18), List((0,0))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceOneLikeTwo() {
    p = new Player(0,0)
    try {
      b.place(p, Move(One(), (0,0), List((0,0), (0,1))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def cannotPlaceTwoOnDiagonal() {
    p = new Player(0,0)
    try {
      b.place(p, Move(Two(), (0,0), List((0,0), (1,1))))
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e=> throw e
    }
  }

  @Test
  def availableCorners_blankBoard() {
    p = new Player(0,0)
    assertEquals(List((0,0)), b.getAvailableCorners(p))
  }

  @Test
  def availableCorners_afterPlacingOne() {
    p = new Player(0,0)
    b.place(p, Move(One(), (0,0), List((0,0))))
    assertEquals(List((1,1)), b.getAvailableCorners(p))
  }

  @Test
  def availableCorners_afterPlacingT() {
    p = new Player(0,0)
    b.place(p, Move(LongT(), (0,0), List((0,0), (1,0), (2,0), (1,1), (1,2))))
    assertEquals(Set((0,3),(2,3),(3,1)), b.getAvailableCorners(p).toSet)
  }

  @Test
  def availableCorners_fullBoard() {
    val all = (for(i<-0 to Board.size-1) yield (for(j<-0 to Board.size-1) yield (i,j))).flatten
    b.assign(p, all)
    assertTrue(b.getAvailableCorners(p).isEmpty)
  }

  @Test
  def blankBoard_toString() {
    assertNotNull(b.toString)
  }

  @Test
  def afterMove_isSeparate() {
    val move = Move(One(), (0,0), List((0,0)))
    val copy = b.afterMove(p, move)
    b.place(p, move)
  }

  @Test
  def afterMove_isEqual() {
    val move = Move(One(), (0,0), List((0,0)))
    b.place(p, move)

    val copy = b.afterMove(p, Move(Two(), (1,1), List((1,1), (1,2))))

    try {
      copy.place(p, move)
      fail()
    }
    catch {
      case e:IllegalPlacementException =>
      case e => throw e
    }
  }

  @Test
  def accessibleGraph_emptyBoard_allCellsAreVertices() {
    val g = b.accessibleGraph(p)
    assertEquals(20*20, g.getVertexCount)
  }

  @Test
  def accessibleGraph_emptyBoard_playerCornerHas3Edges() {
    val g = b.accessibleGraph(p)
    assertEquals(3, g.getOutEdges(p.corner).size)
  }

  @Test
  def accessibleGraph_emptyBoard_playerCornerHasStraightEdgesToSides() {
    val g = b.accessibleGraph(p)
    g.findEdge((0,0), (0,1)) match {
      case Straight(_, _) =>
      case _ => fail()
    }
    g.findEdge((0,0), (1,0)) match {
      case Straight(_, _) =>
      case _ => fail()
    }
  }

  @Test
  def accessibleGraph_emptyBoard_playerCornerHasDiagonalEdgesToCorner() {
    val g = b.accessibleGraph(p)
    g.findEdge((0,0), (1,1)) match {
      case Diagonal(_, _) =>
      case _ => fail()
    }
  }

  @Test
  def accessibleGraph_emptyBoard_distanceToOppositeCorner() {
    val g = b.accessibleGraph(p)
    val algo = new UnweightedShortestPath(g)
    assertEquals(19, algo.getDistance((0,0), (19,19)))    
  }

  @Test
  def accessibleGraph_invalidCells_notVertices() {
    b.place(p, Move(One(), (0,0), List((0,0))))
    val g = b.accessibleGraph(p)
    assertEquals(20*20-3, g.getVertexCount)
  }

}