
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
     * Metodo de calculo de longitud usando foldRight
     * @param l
     * @tparam Int
     * @return
     */
   def lenght[Int](l : List[Int]) = {
      foldRight(l, 0)((x,y) => y+1)
   }

   /**
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
         println("Valor de z: "+z+" acum: "+acum)
         l match {
            case Nil => acum
            case Cons(x, xs) => go(xs, z, f(x, acum))
         }
      }

      // Se desencadena el funcionamiento de la funcion
      go(l, z, z)
   }

   /**
     * Metodo de suma usando foldLeft
     * @param l
     * @return
     */
   def sum3(l : List[Int]) = {
      foldLeft(l, 0.0)(_ + _)
   }

   /**
     * Metodo de multiplicacion usando foldLeft
     * @param l
     * @return
     */
   def product3(l : List[Double]) = {
      foldLeft(l, 1.0)(_ * _)
   }

   /**
     * Metodo de calculo de la longitud en base a foldLeft
     * @param l
     * @tparam Int
     * @return
     */
   def length2[Int](l : List[Int]) = {
      foldLeft(l, 0)((x,y) => y+1)
   }

   def reverse[A](l : List[A]) : List[A] = {
      foldLeft(l, Nil)((l1, l2) => {
         l1 match {
            case Nil => l2
            case Cons(h, t) => Cons()
         }
      })
   }
}

/**
  * Objeto prueba, derivando de App para probar
  */
object Prueba extends App {
   val listaEnteros1 = List[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   val res1 = List.sum(listaEnteros1)
   println("Resultado de suma: " + res1)

   val listaEnteros2 = Nil
   val res2 = List.sum(listaEnteros2)
   println("Resultado de suma (lista vacia): " + res2)

   // Producto sobre la lista no vacia
   val listaEnteros3 = List[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
   val res3 = List.product(listaEnteros3)
   println("Resultado de producto: " + res3)

   val res4 = List.product(listaEnteros2)
   println("Resultado de producto (lista vacia): " + res4)

   // Prueba de tail
   val res5 = List.tail(listaEnteros1)
   println(res5)

   // Prueba de drop
   val res6 = List.drop(listaEnteros1, 3)
   println(res6)

   // Prueba de drop sobre la lista vacia
   val res7 = List.drop(Nil, 3)
   println(res7)

   // Prueba de dropWhile
   val res8 = List.dropWhile(listaEnteros1, (x: Int) => x < 3)
   println(res8)

   // Prueba de setHead
   val res9 = List.setHead(-10, listaEnteros1)
   println(res9)

   // Prueba de init
   val res10 = List.init(listaEnteros1)
   println(res10)

   // Prueba de sum2
   val res11 = List.sum2(listaEnteros1)
   println(res11)

   // Prueba de mul2
   val res12 = List.product2(listaEnteros3)
   println(res12)

   // Prueba del ejercicio 8
   val res13:List[Int] = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
   println(res13)

   // Prueba del ejercicio 10
   val res14 = List.lenght(listaEnteros1)
   println(res14)

   // Prueba del ejercicio 11
   val res15 = List.sum3(listaEnteros1)
   println(res15)

   // Igual con product3
   val res16 = List.product3(listaEnteros3)
   println(res16)

   // Igual con length2
   val res17 = List.length2(listaEnteros1)
   println(res17)
}
