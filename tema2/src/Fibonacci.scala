
/**
  * Created by mgomez on 1/3/17.
  */
object Fibonacci {

   /**
     * Ejercicio 1: implementacion del metodo para obtener los valores que
     * componen la serie de Fibinacci
     * @param n indice del valor a obtener
     */
      def fib(n: Int): Int = {
         // Caso base: los terminos con indice 0 y 1 tienen valor 1
         if(n == 0 || n == 1) n
         else
            // Caso contrario, el termino se obtiene como la suma de los
            // dos terminos anteriores
            fib(n-2)+fib(n-1)
      }

   /**
     * Otras version para generacion de valores de la serie de Fibonacci,
     * pero asegurando la condicion de tail recursion
     * @param n
     * @return
     */
   def fibTailRecursion(n : Int) : Int = {
      @annotation.tailrec
      def go(n: Int, act : Int, sig : Int) : Int = {
         if(n == 0) act
         else go(n-1, sig, sig+act)
      }

      // Llamada a go para desencadenar el proceso
      go(n, 0, 1)
   }

      // test implementation of `fib`
      def main(args: Array[String]): Unit = {
         println("Esperado: 0, 1, 1, 2, 3, 5, 8")
         println("Real:   %d, %d, %d, %d, %d, %d, %d".format(
            fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))
         println("Real (TR):   %d, %d, %d, %d, %d, %d, %d".format(
            fibTailRecursion(0), fibTailRecursion(1), fibTailRecursion(2), fibTailRecursion(3),
            fibTailRecursion(4), fibTailRecursion(5), fibTailRecursion(6)))
      }
}
