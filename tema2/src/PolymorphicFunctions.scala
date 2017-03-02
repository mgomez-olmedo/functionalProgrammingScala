
/**
  * Created by mgomez on 1/3/17.
  */
object PolymorphicFunctions {
   // Here's a polymorphic version of `binarySearch`, parameterized on
   // a function for testing whether an `A` is greater than another `A`.
   /**
     * Metodo polimorfico (paramterizado) para busqueda binaria. Trabaja
     * con colecciones de cualquier tipo (representado por A)
     * @param coleccion
     * @param aBuscar
     * @param mayorQue
     * @tparam A
     * @return valor negativo si no se encuentra el valor o el indice de la
     *         posicion en que se encuentra el valor
     */
   def binarySearch[A](coleccion: Array[A], aBuscar: A, mayorQue: (A, A) => Boolean): Int = {
      /**
        * Funcion auxiliar para hacer la busqueda binaria de forma recursiva
        * con tail - recursion
        * @param izquierda
        * @param centro
        * @param derecha
        * @return
        */
      @annotation.tailrec
      def go(izquierda: Int, centro: Int, derecha: Int): Int = {
         // Caso base: se cruzan los limites izquiero y derecho
         if (izquierda > derecha) -centro - 1
         else {
            // Caso inductivo

            // Se determina el centro nuevo
            val centro2 = (izquierda + derecha) / 2

            // Se determina el valor central
            val valorCentro = coleccion(centro2)

            // Se determina si el valor del centro es mayor que el valor
            // a buscar
            val mayor = mayorQue(valorCentro, aBuscar)

            // Si hay igualdad, se devuelve la posicion central
            if (!mayor && !mayorQue(aBuscar, valorCentro)) centro2
            else
               // Si el valor del centro es mayor que el valor a buscar
               // entonces seguimos buscando a la izquierda
               if (mayor) go(izquierda, centro2, centro2 - 1)
               else
                  // En caso contrario, se busca a la derecha
                  go(centro2 + 1, centro2, derecha)
         }
      }

      // Se inicia la busqueda
      go(0, 0, coleccion.length - 1)
   }

   def main(args: Array[String]): Unit = {
      val arrayInt : Array[Int] = Array(1, 5, 20, 35, 57, 98, 123, 215);
      val mayorQueInt : (Int, Int) => Boolean = (x,y) => (x>y);

      // Se prueba el metodo con un valor que exista
      val res1=PolymorphicFunctions.binarySearch(arrayInt, 98, mayorQueInt)
      println(res1)

      // Siguiente prueba: valor final
      val res2=PolymorphicFunctions.binarySearch(arrayInt, 215, mayorQueInt)
      println(res2)

      // Siguiente prueba: valor inicial
      val res3=PolymorphicFunctions.binarySearch(arrayInt, 1, mayorQueInt)
      println(res3)

      // Prueba con valor que no se encuentra
      val res4=PolymorphicFunctions.binarySearch(arrayInt, 300, mayorQueInt)
      println(res4)
   }
}
