package fpinscala.gettingstarted

/**
  * Created by mgomez on 1/3/17.
  */
object Ejercicios {
   /**
     * Ejercicio 2: funcion polimorfica para comprobar si un array
     * esta ordenado
     *
     * @param as
     * @param gt
     * @tparam A
     * @return
     */
   def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
      def go(indice: Int): Boolean = {
         if (indice == as.length - 2) gt(as(indice), as(indice + 1))
         else if (!gt(as(indice), as(indice + 1))) false
         else go(indice + 1)
      }

      go(0)
   }

   // Polymorphic functions are often so constrained by their type
   // that they only have one implementation! Here's an example:

   def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
      (b: B) => f(a, b)

   // Exercise 3: Implement `curry`.

   // Note that `=>` associates to the right, so we could
   // write the return type as `A => B => C`
   def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      (a : A) => (b : B) => f(a, b)
   }

   // NB: The `Function2` trait has a `curried` method already

   // Exercise 4: Implement `uncurry`
   def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a : A, b : B) => f(a)(b)
   }

   /*
   NB: There is a method on the `Function` object in the standard library,
   `Function.uncurried` that you can use for uncurrying.

   Note that we can go back and forth between the two forms. We can curry
   and uncurry and the two forms are in some sense "the same". In FP jargon,
   we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
   a term we inherit from category theory.
   */

   // Exercise 5: Implement `compose`

   //def compose[A, B, C](f: B => C, g: A => B): A => C = {
   //   ???
   //}

   def main(args: Array[String]): Unit = {
      val array1 : Array[Int] = Array(1, 5, 20, 35, 57, 98, 123, 215);
      val array2 : Array[Int] = Array(100, 5, 20, 35, 57, 98, 123, 215);

      val res1=Ejercicios.isSorted(array1, (x : Int,y : Int) => (x < y))
      println("Sobre array ordenado: "+res1)
      val res2=Ejercicios.isSorted(array2, (x : Int,y : Int) => (x < y))
      println("Sobre array no ordenado: "+res2)
   }
}
