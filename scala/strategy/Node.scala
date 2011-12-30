package blokus

import heuristic.Heuristic

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 3, 2010
 * Time: 9:02:36 PM
 * To change this template use File | Settings | File Templates.
 */

class Node(
        val board: Board,
        val players: Seq[PlayerState],
        val current: PlayerState,
        val move: Move = null
) {

  def children(): Seq[Node] = {
    current.player.legalMoves(board, prefilter(current.pieces)).map(move => {
      val nextBoard = board.afterMove(current.player, move)
      val nextPlayers = players.map(_ match {
        case p if p == current => p - move.piece
        case p => p
      })
      val nextCurrent = players.find(p => p.player == current.player.next).get

      new Node(
        nextBoard,
        nextPlayers,
        nextCurrent,
        move
      )
    })
  }

  def prefilter(pieces: Set[Piece]): Set[Piece] = {
    if (pieces.size > 17) {
      // only consider 5-pieces in the first 4 turns
      if (pieces.size > 19)
        // don't consider even specific 5-pieces in the first 2 turns
        pieces.filter(p => p.size > 4 && !Node.toSaveList.contains(p))
      else
        pieces.filter(p => p.size > 4)
    }
    else
      pieces
  }

  def initialScores(heuristic: Heuristic): Map[Player,Int] = {
    scores(_ match {
      case p if p == current => heuristic.worstScore
      case _ => heuristic.bestScore
    })
  }

  def realScores(): Map[Player,Int] = {
    scores(p => p.player.score(p.pieces))
  }

  def terminalScores(heuristic: Heuristic): Map[Player,Int] = {
    val reals = realScores
    val bestScore = reals.values.max
    scores(_ match {
      case p if reals(p.player) == bestScore => heuristic.bestScore
      case _ => heuristic.worstScore
    })
  }

  def heuristicScores(heuristic: Heuristic): Map[Player,Int] = {
    scores(p => heuristic.value(this, p.player))
  }

  private def scores(score: PlayerState => Int): Map[Player,Int] = {
    Map(players.map(p => (p.player, score(p))): _ *)
  }

}

object Node {

  def apply(board: Board, player: Player): Node = {
    var players = player :: Nil
    var n = player.next
    while (n != player) {
      players = n :: players
      n = n.next
    }

    val playerStates = players.reverse.map(
      p => new PlayerState(p, p.pieces.toSet)
    )

    val current = playerStates.find(p => p.player == player).get

    new Node(board, playerStates, current)
  }

  private val longI = LongI()
  private val longL = LongL()
  private val x = X()
  private val u = U()
  val toSaveList = List(longI, longL, x, u)

}