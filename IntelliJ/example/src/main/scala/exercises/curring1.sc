//coursera course: Functional Programming Principles in Scala
//link: https://www.coursera.org/learn/progfun1/home/week/2

//tail recursive of sum
object exercise1 {

  // can we use a function generalized the sum and product?
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if(a > b) zero //unit value like the 1 before
    else f(a) * mapReduce(f, combine ,1)(a+1, b)

  def product(f: Int => Int)( a: Int, b: Int): Int = mapReduce(f, (x, y) => x*y, 1)(a,b)
  product(x => x*x)(3,4)

  def fact(n: Int) = product(x=>x)(1,n)
  fact(5)

}
