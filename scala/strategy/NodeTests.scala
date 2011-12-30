package blokus

import heuristic.Heuristic
import org.junit.{Assert, Test, Before}
import org.specs.Specification
import org.specs.mock.Mockito

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 4, 2010
 * Time: 12:24:47 PM
 * To change this template use File | Settings | File Templates.
 */

class NodeTests extends Specification with Mockito {

  var node: Node = null
  var board: Board = null
  var player1: Player = null
  var ps1: PlayerState = null
  var player2: Player = null
  var ps2: PlayerState = null

  @Before
  def setUp() {
    board = mock[Board]
    player1 = mock[Player]
    player2 = mock[Player]
    ps1 = new PlayerState(player1, Set(One(), Two()))
    ps2 = new PlayerState(player2, Set(One(), Two()))
    player1.next returns player2
    player2.next returns player1

    node = new Node(board, List(ps1, ps2), ps1)
  }

  @Test
  def children_setsCurrentToNext() {
    val move = Move(One(), (0,0), List((0,0)))
    player1.legalMoves(any[Board], any[Set[Piece]]) returns List(move)

    val child :: Nil = node.children
    Assert.assertEquals(player2, child.current.player)
  }

  @Test
  def children_setsBoardToNext() {
    val move = Move(One(), (0,0), List((0,0)))
    player1.legalMoves(any[Board], any[Set[Piece]]) returns List(move)

    val nextBoard = mock[Board]
    board.afterMove(player1, move) returns nextBoard

    val child :: Nil = node.children
    Assert.assertEquals(nextBoard, child.board)
  }

  @Test
  def children_savesMove() {
    val move = Move(One(), (0,0), List((0,0)))
    player1.legalMoves(any[Board], any[Set[Piece]]) returns List(move)

    val nextBoard = mock[Board]
    board.afterMove(player1, move) returns nextBoard

    val child :: Nil = node.children
    Assert.assertEquals(move, child.move)
  }

  @Test
  def children_removesPieceFromPlayer() {
    val move = Move(One(), (0,0), List((0,0)))
    player1.legalMoves(any[Board], any[Set[Piece]]) returns List(move)

    val nextBoard = mock[Board]
    board.afterMove(player1, move) returns nextBoard

    val child :: Nil = node.children
    Assert.assertEquals(Set(Two()), child.players.find(p => p.player == player1).get.pieces)
  }

  @Test
  def initialScores() {
    val heuristic = mock[Heuristic]
    heuristic.bestScore returns 100
    heuristic.worstScore returns 0

    val scores = node.initialScores(heuristic)
    Assert.assertEquals(0, scores(player1))
    Assert.assertEquals(100, scores(player2))
  }

  @Test
  def terminalScores() {
    val heuristic = mock[Heuristic]
    heuristic.bestScore returns 100
    heuristic.worstScore returns 0

    player1.score(any[Set[Piece]]) returns 80
    player2.score(any[Set[Piece]]) returns 70

    val scores = node.terminalScores(heuristic)
    Assert.assertEquals(100, scores(player1))
    Assert.assertEquals(0, scores(player2))
  }

  @Test
  def heuristicScores() {
    val heuristic = mock[Heuristic]
    heuristic.value(node, player1) returns 40
    heuristic.value(node, player2) returns 50

    val scores = node.heuristicScores(heuristic)
    Assert.assertEquals(40, scores(player1))
    Assert.assertEquals(50, scores(player2))
  }

  @Test
  def construct() {
    player1.pieces returns scala.collection.mutable.Set(One(), Two(), CrookedThree())
    player2.pieces returns scala.collection.mutable.Set(Two())

    val n = Node(board, player1)

    Assert.assertEquals(board, n.board)
    Assert.assertEquals(player1, n.current.player)
    Assert.assertEquals(2, n.players.size)
    Assert.assertEquals(Set(One(), Two(), CrookedThree()), n.players.find(p => p.player == player1).get.pieces)
    Assert.assertEquals(Set(Two()), n.players.find(p => p.player == player2).get.pieces)
  }

}