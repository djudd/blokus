package blokus.heuristic

import blokus.{Player, Node}

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 8, 2010
 * Time: 8:39:10 AM
 *
 * Should always return a value between 0 and 1000 (inclusive).
 *
 */

trait Calculator {

  def value(node: Node, player: Player): Int  

}