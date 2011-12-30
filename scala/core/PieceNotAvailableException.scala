package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 27, 2010
 * Time: 8:27:38 PM
 * To change this template play File | Settings | File Templates.
 */

class PieceNotAvailableException(val msg:String) extends RuntimeException(msg) {
  
}