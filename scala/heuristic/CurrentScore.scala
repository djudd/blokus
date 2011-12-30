package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 4, 2010
 * Time: 1:17:43 PM
 * To change this template use File | Settings | File Templates.
 */

class CurrentScore extends Calculator {

  def value(node: Node, player: Player): Int = {
    val pieces = node.players.find(p => p.player == player).get.pieces
    return player.score(pieces)
  }

}