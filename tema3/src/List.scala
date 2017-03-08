
/**
  * Trait parametrizado para representar las listas. Se usa la indicacion de covarianza
  * para que si A deriva de B, entonces List[A] derive de List[B]. Se agrega sealed para
  * evitar que pueda modificarse esta declaracion fuera del archivo. En definitiva, esta
  * declaracion permite crear el tipo List
  *
  * @tparam A
  */
sealed trait List[+A]

/**
  * Objeto case para representar la lista vacia. Se crea como parametrizada en
  * Nothing, que es subtipo de todos los tipos
  */
case object Nil extends List[Nothing]

/**
  * Clase case para la lista no vacia. Se declara de esta forma para poder
  * almacenar los datos que componen la lista: head y tail. Tanto esta definicion
  * como la anterior ofrecen las formas de construccion de una lista (se usa
  * Cons como abreviatura de constructor)
  *
  * @param head
  * @param tail
  * @tparam A
  */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
  * Companion object para el tipo List. De esta forma permite crear metodos
  * de utilidad para el tipo. De esta forma, podemos tener metodos de utilidad
  * sobre el tipo que estan reunidos en un modulo que se llama como el tipo
  * asociado
  */
object List {
   /**
     * Metodo de suma de listas. Se usa pattern matching para determina si
     * se trata de la lista vacia o de lista con contenido. Se observa que
     * se declara para enteros. Tambien el uso de match
     *
     * @param ints
     * @return
     */
   def sum(ints: List[Int]): Int = ints match {
      // La suma para la lista vacia es 0
      case Nil => 0
      // La suma para la lista con contenido se va obteniendo de forma recursiva
      case Cons(x, xs) => x + sum(xs)
   }

   /**
     * Metodo para obtener el producto de los elementos de la lista. Se observa
     * que se declara para valores de tipo Double. Uso de match
     *
     * @param ds
     * @return
     */
   def product(ds: List[Double]): Double = ds match {
      // Producto de lista vacia
      case Nil => 1.0
      // Producto si el primer elemento es 0
      case Cons(0.0, _) => 0.0
      // Caso general: se acumula multiplicando con el valor en cabeza
      case Cons(x, xs) => x * product(xs)
   }

   /**
     * Ejercicio 2
     * Metodo para eliminar el primer elemento de la lista
     *
     * @param ds
     * @tparam A
     * @return
     */
   def tail[A](ds: List[A]): List[A] = {
      ds match {
         case Nil => Nil
         case Cons(_, t) => t
      }
   }

   /**
     * Ejercicio 3
     * Metodo para eliminar n elementos de la cabeza de la lista
     *
     * @param ds
     * @param n
     * @tparam A
     * @return
     */
   def drop[A](ds: List[A], n: Int): List[A] = {
      if (n <= 0) ds
      else {
         ds match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
         }
      }
   }

   /**
     * Ejercicio 4
     * Metodo para eliminar elementos de la cabeza mientras se cumpla
     * una determinada condicion
     *
     * @param ds
     * @param f
     * @tparam A
     * @return
     */
   def dropWhile[A](ds: List[A], f: A => Boolean): List[A] = {
      ds match {
         case Cons(h, ts) if (f(h)) => dropWhile(ts, f)
         case _ => ds
      }
   }

   /**
     * Ejercicio 5
     * Metodo para agregar un elemento en el inicion de la lista
     * @param a
     * @param as
     * @tparam A
     * @return
     */
   def setHead[A](a: A, as:List[A]) : List[A] = {
      as match{
         case Nil => Cons(a, Nil)
         case _ => Cons(a, as)
      }
   }

   /**
     * Metodo que permite crear objetos sin usar el nombre del metodo,
     * basta con poner List y una lista variable de argumentos... Ese
     * es el significado del asterisco (variadic methods)
     *
     * @param as
     * @tparam A
     * @return
     */
   def apply[A](as: A*): List[A] =
   // Si la lista esta vacia, se devuelve Nil
      if (as.isEmpty) Nil
      // En caso contrario, se crea usando Cons, indicando la cabeza
      // de la lista y la lista que se crea con la cola
      else Cons(as.head, apply(as.tail: _*))

   /**
     * Metodo para agregar una lista completa al final de otra
     * @param a1
     * @param a2
     * @tparam A
     * @return
     */
   def append[A](a1 : List[A], a2 : List[A]) : List[A] = {
      a1 match {
         case Nil => a2
         case Cons(h,t) => Cons(h, append(t, a2))
      }
   }

   /**
     * Ejercicio 6
     * Metodo para eliminar el ultimo elemento de la lista
     * @param as
     * @tparam A
     * @return
     */
   def init[A](as : List[A]) : List[A] = {
      as match {
         case Cons(h, Nil) => Nil
         case Cons(h, t) =>  Cons(h, init(t))
      }
   }

   /**
     * Metodo foldRight para tratamiento de listas con una determinada
     * funcion, aplicada paso a paso
     * @param l lista a tratar
     * @param z valor inicial a considerar
     * @param f funcion a aplicar
     * @tparam A
     * @tparam B
     * @return
     */
   def foldRight[A,B](l : List[A], z : B)(f : (A,B) => B) : B = {
      l match {
         case Nil => z
         case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
   }

   /**
     * Metodo de suma usando foldRight
     * @param l
     * @return
     */
   def sum2(l : List[Int]) = {
      foldRight(l, 0.0)(_ + _)
   }

   /**
     * Metodo de producto usando foldRight
     * @param l
     * @return
     */
   def product2(l : List[Double]) =
      foldRight(l, 1.0)(_ * _)

   /**
     * Ejercicio 9
     * Metodo de calculo de longitud usando foldRight
     * @param l
     * @tparam Int
     * @return
     */
   def lenght[Int](l : List[Int]) = {
      foldRight(l, 0)((x,y) => y+1)
   }

   /**
     * Ejercicio 10
     * Metodo foldLeft para tratamiento de una lista, pero ahora con
     * tail recursion
     * @param l
     * @param z
     * @param f
     * @tparam A
     * @tparam B
     * @return
     */
   def foldLeft[A,B](l : List[A], z : B)(f : (A,B) => B) : B = {
      def go(l : List[A], z : B, acum : B) : B = {
         l match {
            case Nil => acum
            case Cons(x, xs) => go(xs, z, f(x, acum))
         }
      }

      // Se desencadena el funcionamiento de la funcion
      go(l, z, z)
   }

   /**
     * Ejercicio 11 (a)
     * Metodo de suma usando foldLeft
     * @param l
     * @return
     */
   def sum3(l : List[Int]) = {
      foldLeft(l, 0.0)(_ + _)
   }

   /**
     * Ejercicio 11 (b)
     * Metodo de multiplicacion usando foldLeft
     * @param l
     * @return
     */
   def product3(l : List[Double]) = {
      foldLeft(l, 1.0)(_ * _)
   }

   /**
     * Ejercicio 11 (c)
     * Metodo de calculo de la longitud en base a foldLeft
     * @param l
     * @tparam Int
     * @return
     */
   def length2[Int](l : List[Int]) = {
      foldLeft(l, 0)((x,y) => y+1)
   }

   /**
     * Ejercicio 12
     * Metodo reverse para invertir el contenido de la lista
     * @param l
     * @tparam A
     * @return
     */
   def reverse[A](l : List[A]) : List[A] = {
      def go(l : List[A], res : List[A]) : List[A] = {
         l match {
            case Nil => res
            case Cons(h,t) => go(t, Cons(h, res))
         }
      }

      go(l,Nil)
   }

   /**
     * Ejercicio 13: foldLeft en terminos de foldRight
     * @param l
     * @param z
     * @param f
     * @tparam A
     * @tparam B
     * @return
     */
   def foldLeftViaFoldRightBase[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

   /**
     * Ejercicio 13: foldLeft en terminos de foldRight
     * @param l
     * @param z
     * @param f
     * @tparam A
     * @tparam B
     * @return
     */
   def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
      val g = (b:B) => b
      foldRight(l, g)((a, g) => {
         b => {
            println("b: "+b+" a: "+a)
            val fba = f(b,a)
            val gba = g(fba)
            println("Calculo de f: "+fba)
            println("Calculo de g: "+gba)
            gba
         }
      })(z)
   }

   /**
     * Ejercicio 13 (b)
     * Metodo de suma usando foldLeft2
     * @param l
     * @return
     */
   def sum4(l : List[Int]) = {
      foldLeftViaFoldRight(l, 0.0)(_ + _)
   }
}

/**
  * Objeto prueba, derivando de App para probar
  */
object Prueba extends App {
   // Ejercicio 1
   val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4,_)))) => x+y
      case Cons(h,t) => h+List.sum(t)
      case _ => 101
   }
   // Debe obtenerse 3 como valor: el caso con el que concuerda en primer
   // lugar es el tercero
   println("Valor de x: "+x)

   // -----------------------------------------------
   val listaEnteros1 = List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   val res1 = List.sum(listaEnteros1)
   println("Suma sobre lista: ")
   println(listaEnteros1)
   println("Resultado de suma: " + res1)
   println()

   val listaEnteros2 = Nil
   val res2 = List.sum(listaEnteros2)
   println("Resultado de suma (lista vacia): " + res2)
   println()

   // Producto sobre la lista no vacia
   val listaEnteros3 = List[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   println("Producto sobre lista: ")
   println(listaEnteros3)
   val res3 = List.product(listaEnteros3)
   println("Resultado de producto: " + res3)
   println()

   val res4 = List.product(listaEnteros2)
   println("Producto sobre lista: ")
   println(listaEnteros2)
   println("Resultado de producto (lista vacia): " + res4)
   println()

   // Ejercicio 2: prueba de tail
   println("Aplicacion de tail: ")
   println(listaEnteros1)
   val res5 = List.tail(listaEnteros1)
   println(res5)
   println()

   // Ejercicio 3: prueba de drop
   println("Aplicacion de drop: ")
   println(listaEnteros1)
   val res6 = List.drop(listaEnteros1, 3)
   println(res6)
   println()

   // Ejercicio 3: Prueba de drop sobre la lista vacia
   println("Uso de drop sobre lista vacia: ")
   val res7 = List.drop(Nil, 3)
   println(res7)
   println()

   // Ejercicio 4: prueba de dropWhile
   println("Uso de dropWhile: ")
   println(listaEnteros1)
   val res8 = List.dropWhile(listaEnteros1, (x: Int) => x < 3)
   println(res8)
   println()

   // Ejercicio 5: prueba de setHead
   println("Uso de setHead: ")
   println(listaEnteros1)
   val res9 = List.setHead(-10, listaEnteros1)
   println(res9)
   println()

   // Ejercicio 6: prueba de init
   println("Uso de init: ")
   println(listaEnteros1)
   val res10 = List.init(listaEnteros1)
   println(res10)
   println()

   // Ejercicio 10 (a): prueba de sum2
   println("Uso de sum2 (implementacion con foldRight): ")
   println(listaEnteros1)
   val res11 = List.sum2(listaEnteros1)
   println(res11)
   println()

   // Ejercicio 10 (b): prueba de mul2
   println("Uso de product2 (implementacion con foldRight): ")
   println(listaEnteros3)
   val res12 = List.product2(listaEnteros3)
   println(res12)
   println()

   // Ejercicio 7: se mira si la implementacion del producto mediante
   // foldRight puede detenerse de forma inmediata al encontrar un 0.0
   // Creamos una lista para esto. Obviamante, el método no puede
   // deternerse e itera sobre todos los elementos de la lista
   val listaEnteros4= List[Double](1, 2, 0, 4, 5, 6, 7, 8, 9, 10)

   // Ejercicio 8: la salida pone de manifiesto la relacion entre
   // foldRight y la construccion de listas
   val res13:List[Int] = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
   println()
   println("Salida del ejercicio 8")
   println(res13)

   // Ejercicio 9: prueba de obtencion de longitud
   println("Uso de length (implementado con foldRight): ")
   println(listaEnteros1)
   val res14 = List.lenght(listaEnteros1)
   println(res14)
   println()

   // Ejercicio 11 (a): Prueba del ejercicio 11
   println("Uso de sum3 (implementado con foldLeft): ")
   println(listaEnteros1)
   val res15 = List.sum3(listaEnteros1)
   println(res15)
   println()

   // Ejercicio 11(b) : prueba de producto
   println("Uso de product3 (implementado con foldLeft): ")
   println(listaEnteros3)
   val res16 = List.product3(listaEnteros3)
   println(res16)
   println()

   // Ejercicio 11 (c): igual con length2
   println("Uso de length2 (implementado sobre foldLeft): ")
   println(listaEnteros1)
   val res17 = List.length2(listaEnteros1)
   println(res17)
   println()

   // Ejercicio 12: prueba de reverse
   println("Uso de reverse (implementado sobre foldLeft): ")
   println(listaEnteros1)
   val res18 = List.reverse(listaEnteros1)
   println(res18)
   println()


   // Ejercicio 13: prueba de foldLeft2
   println("Uso de sum4 (implementado sobre foldLeft2): ")
   println(listaEnteros1)
   val res19 = List.sum4(listaEnteros1)
   println(res19)
   println()

}
