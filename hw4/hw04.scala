package hw04

object hw04 {

  def list_add(l1: List[Int], l2: List[Int]): List[Int] = {
    (l1.reverse.padTo(l1.length.max(l2.length), 0), l2.reverse.padTo(l1.length.max(l2.length), 0)).zipped.map(_ + _)
  }

  def insort(l: List[Int], m: Int): List[Int] = {
    (l :+ m).sorted
  }

  def ltake(l: List[Int], i: Int): List[Int] = {
    l.takeRight(i)
  }

  def lall(f: Int => Boolean, l: List[Int]): Boolean = {
    l.forall(f)
  }

  def lmap(f: Int => Int, l: List[Int]): List[Int] = {
    l.map(f)
  }

  def lfilter(f: Int => Boolean, l: List[Int]): List[Int] = {
    l.filter(f)
  }

  def ltabulate(n: Int, f: Int => Int): List[Int] = {
    (0 until n).map(f).reverse.toList
  }

  def lrev(l: List[Int]): List[Int] = {
    l.reverse
  }

  def lconcat(l: List[List[Int]]): List[Int] = {
    l.flatten
  }

  def lfoldr(f: (Int, Int) => Int, l: List[Int], e: Int): Int = {
    l.foldRight(e)(f)
  }

  def lzip(a: List[String], b: List[Int]): List[(String, Int)] = {
    a.reverse.zip(b.reverse).reverse
  }

  def split(l: List[Int]): (List[Int], List[Int]) = {
    l.zipWithIndex.partition { case (_, index) => index % 2 == 0 } match {
      case (evenIndexed, oddIndexed) =>
        (evenIndexed.map { case (value, _) => value }, oddIndexed.map { case (value, _) => value })
    }
  }

  def cartprod(l1: List[Int], l2: List[Int]): List[(Int, Int)] = {
    l1.flatMap(x => l2.map(y => (x,y)))
  }


  def main(args: Array[String]): Unit = {
    println("** p1 **")
    println(list_add(List(1, 2, 3), List(1, 2, 3, 4, 5)))
    println()

    println("** p2 **")
    println(insort(List(1, 2, 4, 5), 3))
    println()

    println("** p3 **")
    println(ltake(List(1, 2, 3, 4), 2))
    println()

    println("** p4 **")
    println(lall(x => x > 0, List(1, 2, 3, 4)))
    println()

    println("** p5 **")
    println(lmap(x => x + 1, List(1, 2, 3)))
    println()

    println("** p6 **")
    println(lfilter(x => x > 2, List(1, 2, 3)))
    println()

    println("** p7 **")
    println(ltabulate(4, (x:Int) => x * x))
    println()

    println("** p8 **")
    println(lrev(List(1, 2, 3)))
    println()

    println("** p9 **")
    println(lconcat(List(List(1, 2, 3), List(4, 5, 6))))
    println()

    println("** p10 **")
    println(lfoldr((x:Int, y:Int) => x - y, List(1, 2, 3), 0))
    println()

    println("** p11 **")
    println(lzip(List("A", "B", "C", "D"), List(1, 2, 3, 4, 5, 6)))
    println()

    println("** p12 **")
    println(split(List(1, 2, 3, 4, 5)))
    println()

    println("** p13 **")
    println(cartprod(List(1, 2), List(3, 4, 5)))
    println()
  }
}
