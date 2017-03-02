
package fpinscala.gettingstarted

// Se importa la declaracion del metodo abs en MiModulo (y cualquier otro)
import fpinscala.gettingstarted.MiModulo._

/**
  * Created by mgomez on 1/3/17.
  */
object Factorial {

   /**
     * Metodo para calculo del factorial usando tail - recursion. La anotacion
     * incluida en la declaracion del metodo permite comprobar si realmente se
     * cumple que el metodo es tail - recursive
     * @param n
     * @return
     */
   def factorial(n: Int): Int = {

      // Funcion auxiliar para calcular el factorial. Se incluye la
      // anotacion para comprobar la condicion de tail-recursion
      @annotation.tailrec
      def go(n: Int, acc: Int): Int = {
         if (n <= 0) acc
         else go(n - 1, n * acc)
      }

      // Se llama a la funcion que desencadena la ejecucion
      go(n, 1)
   }

   /**
     * Implementacion del factorial con bucle while y variables mutables
     * @param n
     * @return
     */
   def factorial2(n: Int): Int = {
      var acc = 1
      var i = n
      while (i > 0) {
         acc *= i;
         i -= 1
      }
      acc
   }

   /**
     * Metodo para formatear el valor de un factorial. Es muy similar
     * a la funcion para formatear el valor absoluto. Por eso lo logico
     * seria disponer de una funcion mas generica que reciba como
     * argumento la funcion a aplicar sobre el valor pasado como
     * primer argumento
     * @param n
     * @return
     */
   private def formatFactorial(n: Int) = {
      val msg = "El factorial de %d es %d."
      msg.format(n, factorial(n))
   }

   /**
     * En el main se usa en realidad formatResult, que es general y recibe la
     * funcion a aplicar sobre el valor tambien como argumento
     * @param args
     */
   def main(args: Array[String]): Unit = {
      println(formatResult("valor absoluto ", -42, abs))
      println(formatResult("factorial ", 7, factorial))
   }
}
