package blokus

import scala.collection.mutable.Set
import org.junit.Assert.assertTrue

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 14, 2010
 * Time: 5:01:35 PM
 * To change this template play File | Settings | File Templates.
 */

class Player(
        private val startX:Int,
        private val startY:Int,
        val pieces: Set[Piece] = Set(Piece.all : _*),
        val strategy: Strategy = null,
        val id: Byte = 1
) {

  val ownershipBit: Byte = (1 << (id-1)).toByte
  val validityBit: Byte = (ownershipBit << 4).toByte
  val name: String = Player.name(ownershipBit)
  val corner = (startX, startY)

  assertTrue((ownershipBit & Board.ownershipBits) > 0)
  assertTrue((validityBit & Board.validityBits) > 0)

  var next: Player = null
  var prev: Player = null

  def play(board: Board) {
    val move = strategy.nextMove(this, board) // TOOD: Strategy should only receive a copy of board
    validate(move)

    board.place(this, move)
    pieces.remove(move.piece)
  }

  def validate(move: Move) {
    move.validate

    if(!pieces.contains(move.piece))
      throw new PieceNotAvailableException(name + " does not have " + move.piece)
  }

  override def toString() = name

  def score: Int = score(pieces.toSet)

  def score(pieces: scala.collection.immutable.Set[Piece]): Int = {
    val basic = 89 - pieces.foldLeft(0)({case (total, piece) => total + piece.size})
    if (pieces.isEmpty)
      return basic + 15
    else
      return basic
  }

  def canPlay(board: Board) = !legalMoves(board).isEmpty

  def legalMoves(board: Board) : Seq[Move] = {
    legalMoves(board, pieces)
  }

  def legalMoves(board: Board, pieces: Iterable[Piece]): Seq[Move] = {
    legalMoves(board, pieces.toList.sortBy(-_.size))
  }

  def legalMoves(board: Board, pieces: List[Piece]) : Seq[Move] = {
    board.getAvailableCorners(this).flatMap(
      corner => pieces.flatMap(
        piece => piece.placements.map(
          placement => new Move(piece, corner, placement)
        )
      )
    ).filter(move => board.onBoardAndValidFor(move.cells, this))
  }

}

object Player {
  def name(bit: Byte): String = {
    bit match {
      case 1 => "b"
      case 2 => "y"
      case 4 => "r"
      case 8 => "g"
    }
  }

}