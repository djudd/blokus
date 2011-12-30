package blokus

import org.junit.Assert.assertTrue
import org.specs.mock.Mockito
import org.specs.Specification
import org.junit.{Before, Test};
import org.junit.Assert.assertFalse;
import org.junit.Assert.assertEquals;


/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 5:02:55 PM
 * To change this template play File | Settings | File Templates.
 */

class PlayerTests extends Specification with Mockito {

  var p: Player = null
  var b: Board = null
  var strategy: Strategy = null

  @Before
  def setUp() {
    strategy = mock[Strategy]
    p = new Player(0,0,strategy=strategy)
    b = mock[Board]
  }

  @Test
  def zeroZeroIsMyCorner() {
    assertTrue((0,0).equals(p.corner))
  }

  @Test
  def nineteenNineteenIsMyCorner() {
    val p = new Player(19,19)
    assertTrue((19,19).equals(p.corner))
  }

  @Test
  def newPlayerHas21Pieces() {
    assertEquals(21, p.pieces.size)
  }

  @Test
  def playerAfterPlayingPieceHas20() {
    strategy.nextMove(p,b) returns Move(One(), (0,0), List((0,0)))

    p.play(b)

    assertEquals(20, p.pieces.size)
  }

  @Test
  def playerCannotPlayPieceAlreadyUsed() {
    strategy.nextMove(p,b) returns Move(One(), (0,0), List((0,0)))

    p.play(b)
    try {
      p.play(b)
      fail()
    }
    catch {
      case e:PieceNotAvailableException =>
      case e => throw e
    }
  }

  @Test
  def playerPlacesPieceOnBoard() {
    val move = Move(One(), (0,0), List((0,0)))

    strategy.nextMove(p,b) returns move

    p.play(b)

    there was one(b).place(p, move)
  }

  @Test
  def score_initial() {
    assertEquals(0, p.score)
  }

  @Test
  def score_afterPlayingOne() {
    strategy.nextMove(p,b) returns Move(One(), (0,0), List((0,0)))

    p.play(b)
    assertEquals(1, p.score)
  }

  @Test
  def score_afterPlayingAll() {
    assertEquals(89+15, p.score(Set.empty))
  }

  @Test
  def score_afterPlayingAllButOne() {
    assertEquals(88, p.score(Set(One())))
  }

  @Test
  def noAvailableCorners_noLegalMoves() {
    b.getAvailableCorners(p) returns Nil

    assertTrue(p.legalMoves(b).isEmpty)
  }

  @Test
  def noValidCells_noLegalMoves() {
    b.getAvailableCorners(p) returns (0,0) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns false

    assertTrue(p.legalMoves(b).isEmpty)
  }

  @Test
  def noPiecesLeft_noLegalMoves() {
    val p = new Player(0,0, pieces = scala.collection.mutable.Set())

    b.getAvailableCorners(p) returns (0,0) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns false

    assertTrue(p.legalMoves(b).isEmpty)
  }

  @Test
  def onePiece_emptyBoard_oneValidMove() {
    val p = new Player(0,0, pieces = scala.collection.mutable.Set(One()))

    b.getAvailableCorners(p) returns (0,0) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns true

    assertEquals(1, p.legalMoves(b).size)
  }

  @Test
  def onePiece_twoAvailableCorners_twoAvailableMoves() {
    val p = new Player(0,0, pieces = scala.collection.mutable.Set(One()))

    b.getAvailableCorners(p) returns (2,3) :: (3,2) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns true

    assertEquals(2, p.legalMoves(b).size)
  }

  @Test
  def twoPiece_oneAvailableCorner_allCellsAvailable_fourAvailableMoves() {
    val p = new Player(0,0, pieces = scala.collection.mutable.Set(Two()))

    b.getAvailableCorners(p) returns (1,1) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns true

    assertEquals(4, p.legalMoves(b).size)
  }

  @Test
  def twoPiece_oneAvailableCorner_belowAboveUnavailable_twoAvailableMoves() {
    val p = new Player(0,0, pieces = scala.collection.mutable.Set(Two()))

    b.getAvailableCorners(p) returns (1,1) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns true
    b.onBoardAndValidFor(List((1,1), (1,0)), p) returns false
    b.onBoardAndValidFor(List((1,1), (1,2)), p) returns false

    assertEquals(2, p.legalMoves(b).size)
  }

  @Test
  def oneAndTwoPieces_oneAvailableCorner_fiveAvailableMoves() {
    val p = new Player(0,0, pieces = scala.collection.mutable.Set(One(), Two()))

    b.getAvailableCorners(p) returns (1,1) :: Nil
    b.onBoardAndValidFor(any[List[(Int,Int)]], any[Player]) returns true

    assertEquals(5, p.legalMoves(b).size)
  }

  @Test
  def name_red() {
    val p = new Player(0,0, id=3)
    assertEquals("r", p.name)
  }

}