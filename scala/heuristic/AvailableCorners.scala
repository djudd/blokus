package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 4, 2010
 * Time: 1:17:43 PM
 * To change this template use File | Settings | File Templates.
 */

class AvailableCorners extends Calculator {

  def value(node: Node, player: Player): Int = node.board.getAvailableCorners(player).size

}