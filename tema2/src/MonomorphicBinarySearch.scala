package fpinscala.gettingstarted

/**
  * Created by mgomez on 1/3/17.
  */
object MonomorphicBinarySearch {

   // First, a binary search implementation, specialized to `Double`,
   // another primitive type in Scala, representing 64-bit floating
   // point numbers
   // Ideally, we could generalize this to work for any `Array` type,
   // so long as we have some way of comparing elements of the `Array`
   def binarySearch(ds: Array[Double], key: Double): Int = {
      @annotation.tailrec
      def go(low: Int, mid: Int, high: Int): Int = {
         if (low > high) -mid - 1
         else {
            val mid2 = (low + high) / 2
            val d = ds(mid2) // We index into an array using the same
            // syntax as function application
            if (d == key) mid2
            else if (d > key) go(low, mid2, mid2 - 1)
            else go(mid2 + 1, mid2, high)
         }
      }

      go(0, 0, ds.length - 1)
   }
}
