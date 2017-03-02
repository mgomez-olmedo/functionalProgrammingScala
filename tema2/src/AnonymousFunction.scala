package fpinscala.gettingstarted

import fpinscala.gettingstarted.MiModulo._
import fpinscala.gettingstarted.Factorial._


/**
  * Created by mgomez on 1/3/17.
  */

/**
  * Clase para uso de funciones anonimas: todas ellas se pasan como
  * argumento a la funcion de nivel superior formatResult
  */
object AnonymousFunction {

   /**
     * Main para probar el uso de funciones anonimas, sin nombre,
     * como argumentos a funcion de nivel superior
     * @param args
     */
   def main(args: Array[String]): Unit = {
      println(formatResult("valor absoluto: ", -42, abs))
      println(formatResult("factorial: ", 7, factorial))
      println(formatResult("incremento: ", 7, (x: Int) => x + 1))

      // Segunda version de expresion lambda para incremento, sin
      // especificar el tipo de x
      println(formatResult("incremento2", 7, (x) => x + 1))

      // Tercera version de expresion lambda, quitando los
      // parentesis de la parte izquierda, al haber un unico argumento
      println(formatResult("incremento3", 7, x => x + 1))

      // Como el argumento se usa una vez y queda claro su tipo por lo
      // que espera recibir la funcion formatResult, se usa el subrayado
      println(formatResult("incremento4", 7, _ + 1))

      // Forma de expresar la funcion anonima como un bloque
      println(formatResult("incremento5", 7, x => {
         val r = x + 1; r
      }))
   }
}
