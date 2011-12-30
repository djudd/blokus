package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 3, 2010
 * Time: 9:03:45 PM
 * To change this template use File | Settings | File Templates.
 */

trait AntagonisticSearchStrategy extends Strategy {

  def nextMove(player: Player, board: Board) : Move = {
    return search(Node(board, player)).move
  }

  def search(node: Node) : Node

}