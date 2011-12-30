package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 30, 2010
 * Time: 7:49:43 PM
 * To change this template use File | Settings | File Templates.
 */

class ManualStrategy(console: Console) extends Strategy {

  override def nextMove(player: Player, board: Board) : Move = {
    return getMove(player, board)
  }

  private def getMove(player: Player, board: Board): Move = {
    val input = console.readline(player.name + " move:")

    try {
      val move = parse(input)
      move.validate
      board.validate(move.cells, player)
      return move
    }
    catch {
      case e => {
        console.write("Invalid move: " + e)
        return getMove(player, board)
      }
    }
  }

  private def parse(input: String): Move = {
    val split = input.replaceAll("\\s", "").split(":")
    val piece = getPiece(split(0))
    val corner = parsePoint(split(1))
    val placement = parsePoints(split(2))

    return Move(piece, corner, placement)
  }

  private def getPiece(name: String): Piece = {
    val clazz = Class.forName("blokus." + name)
    return clazz.newInstance.asInstanceOf[Piece]
  }

  private def parsePoints(input: String): List[(Int,Int)] = {
    val split = input.split(";")
    return split.map(parsePoint).toList
  }

  private def parsePoint(input: String): (Int,Int) = {
    val split = input.split(",")
    return (split(0).toInt, split(1).toInt)
  }

}