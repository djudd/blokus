package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 1, 2010
 * Time: 7:54:29 AM
 * To change this template use File | Settings | File Templates.
 */

trait Heuristic {

  def value(node: Node, player: Player): Int

  val worstScore: Int
  val bestScore: Int

}