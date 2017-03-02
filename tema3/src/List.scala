
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

// A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */

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
    case Nil => 0
    // Producto si el primer elemento es 0
    case Cons(0.0, _) => 0.0
    // Caso general: se acumula multiplicando con el valor en cabeza
    case Cons(x, xs) => x * product(xs)
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
}