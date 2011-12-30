package blokus.core

import org.apache.commons.collections15.Transformer

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Dec 18, 2010
 * Time: 2:28:43 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class Edge
case class Straight(start: (Int,Int), end: (Int,Int)) extends Edge
case class Diagonal(start: (Int,Int), end: (Int,Int)) extends Edge
case class Synthetic(start: (Int,Int), end: (Int,Int)) extends Edge

object Edge {
  val weight = new Transformer[Edge,java.lang.Integer] {
    override def transform(e: Edge): java.lang.Integer = e match {
      case Straight(_,_) => 1
      case Diagonal(_,_) => 2
      case Synthetic(_,_) => 0
    }
  }
}