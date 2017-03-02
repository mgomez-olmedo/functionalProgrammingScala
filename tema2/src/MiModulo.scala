
package fpinscala.gettingstarted

/**
  * Declaracion de objeto, como forma de implementacion del patron Singleton.
  * De esta forma, este objeto sera el unico objeto posible de este tipo.
  * Se observa que el objeto (modulo) cuenta con 3 metodos: abs, formatAbs
  * y main
  */
object MiModulo {
   /**
     * Metodo para calculo del valor absoluto. Se trata de una funcion
     * pura: recibe un argumento como entrada y devuelve un valor
     *
     * @param n
     * @return
     */
   def abs(n: Int): Int =
      if (n < 0) -n
      else n

   /**
     * Metodo privado para formatear el valor absoluto de un valor y
     * genera una cadena con el formato adecuado
     *
     * @param x
     * @return
     */
   private def formatAbs(x: Int) = {
      val msg = "El valor absoluto de %d es %d"
      msg.format(x, abs(x))
   }

   // We can generalize `formatAbs` and `formatFactorial` to
   // accept a _function_ as a parameter
   /**
     * Metodo que generaliza el metodo anterior y que recibe una
     * funcion como argumento. Por tanto, se trata de una funcion
     * de nivel superior
     * @param name
     * @param n
     * @param f esta funcion se aplica sobre n y es el resultado producido
     *          lo que se muestra por pantalla
     * @return
     */
   def formatResult(name: String, n: Int, f: Int => Int) = {
      val msg = "El %s de %d es %d."
      msg.format(name, n, f(n))
   }

   /**
     * Metodo main para permitir la ejecucion del codigo
     *
     * @param args
     */
   def main(args: Array[String]): Unit =
      println(formatAbs(-42))
}
