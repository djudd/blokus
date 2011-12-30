package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 28, 2010
 * Time: 3:46:37 PM
 * To change this template use File | Settings | File Templates.
 */

trait Strategy {

  def nextMove(player: Player, board: Board) : Move

}