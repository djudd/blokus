package blokus

/**
 * Created by IntelliJ IDEA.
 * User: david
 * Date: Nov 30, 2010
 * Time: 8:18:21 PM
 * To change this template use File | Settings | File Templates.
 */

class Console {

  def write(str: String) {
    println(str)
  }

  def readline(prompt: String) : String = {
    Console.readLine(prompt)
  }
  
}