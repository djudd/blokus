package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 3, 2010
 * Time: 9:15:23 PM
 * To change this template use File | Settings | File Templates.
 */

class PlayerState(val player: Player, val pieces: Set[Piece]) {

  def - (piece: Piece): PlayerState = {
    new PlayerState(player, pieces - piece)
  }

}