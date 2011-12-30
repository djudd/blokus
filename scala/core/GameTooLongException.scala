package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 29, 2010
 * Time: 9:05:34 PM
 * To change this template use File | Settings | File Templates.
 */

class GameTooLongException(val msg: String) extends RuntimeException(msg)