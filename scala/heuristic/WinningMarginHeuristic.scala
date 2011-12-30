package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 4, 2010
 * Time: 4:56:17 PM
 * To change this template use File | Settings | File Templates.
 */

class WinningMarginHeuristic(calculator: Calculator) extends Heuristic {

  def value(node: Node, player: Player): Int = {
    val scores = Map(node.players.map(p => (p.player, calculator.value(node, p.player))): _ *)
    scores(player) - scores.filterKeys(p => p != player).values.max
  }

  val worstScore = -1000 // should be -Inf
  val bestScore = 1000 // should be Inf

}