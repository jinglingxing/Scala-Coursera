//coursera course: Functional Programming Principles in Scala
//link: https://www.coursera.org/learn/progfun1/home/week/2

//tail recursive of sum
object exercise1 {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int =
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    loop(a, 0)
  }
  sum(x => x * x, 3, 5)

  def product(f: Int => Int)( a: Int, b: Int): Int =
    if(a > b) 1
    else f(a) * product(f)(a+1, b)
  product(x => x*x)(3,4)

  def fact(n: Int) = product(x=>x)(1,n)
  fact(5)

  // can we use a function generalized the sum and product?
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if(a > b) zero //unit value like the 1 before
    else f(a) * mapReduce(f, combine ,zero)(a+1, b)
}
