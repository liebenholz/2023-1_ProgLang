package hw03

object hw3 {

  // 입력 n은 0보다 큰 정수라고 가정합니다.
  def sum(n: Int): Int = {
    // 자기호출 함수를 사용하여 계산합니다.
    if (n<=0) 0
    else n + sum(n-1)
  }

  def fac(n: Int): Int = {
    // n이 1이면 1로 출력합니다
    if (n == 1) 1
    // 그렇지 않으면 재귀함수를 사용하여 계산합니다.
    else n * fac(n - 1)
  }

  def fib(n: Int): Int = n match {
    // n 이 1 또는 2면 1을 출력합니다.
    case 1 => 1
    case 2 => 1
    // n이 3 이상인 경우에는
    case _ => fib(n-1) + fib(n-2)
  }

  // 입력 m과 n은 0 이상이고 m+n은 0보다 크다고 가정합니다.
  def gcd(n: Int, m: Int): Int = {
    // 작은 값과 큰 값을 먼저 출력합니다.
    if(m<=n) println("gcd " + m + " " + n)
    // 유클리드 호제법을 사용하여 자기호출 함수로 계산하세요.
    if(m==0) n
    else gcd(m, n%m)
  }

  // 입력 list는 List(3, 4, 14, 7, 2)와 같은 형태의 정수 리스트라고 가정합니다.
  def max(list: List[Int]): Int = {
    // 정수 리스트에서 가장 큰 값을 반환하는 자기호출 함수를 작성하세요.
    def maxOfList(nums: List[Int], currentMax: Int): Int = {
      if(nums.isEmpty) currentMax
      else(maxOfList(nums.drop(1), if(nums.head > currentMax) nums.head else currentMax))
    }
    // 빈 리스트를 입력받으면 0을 반환하세요.
    if(list.isEmpty) 0
    // 그렇지 않다면..
    else maxOfList(list.drop(1), list.head)
  }

  // p6부터 p8까지 사용할 트리 예시
  sealed trait Tree[Int]
  case class Leaf[Int](elem: Int) extends Tree[Int]
  case class Node[Int](elem: Int, left: Tree[Int], right: Tree[Int]) extends Tree[Int]
  val tree = Node(7, Node(3, Leaf(1), Leaf(2)), Leaf(4))

  // p6
  def sum_tree(t: Tree[Int]): Int = {
    //재귀함수를 이용하여 트리의 합을 구하세요.
    if (t.isInstanceOf[Leaf[Int]]) {
      val leaf = t.asInstanceOf[Leaf[Int]]
      leaf.elem
    } else if (t.isInstanceOf[Node[Int]]) {
      val node = t.asInstanceOf[Node[Int]]
      node.elem + sum_tree(node.left) + sum_tree(node.right)
    } else 0
  }

  // p7
  def depth(t: Tree[Int]): Int = {
    //재귀함수를 이용하여 트리의 깊이를 구하세요.
    if (t.isInstanceOf[Node[Int]]) {
      val node = t.asInstanceOf[Node[Int]]
      val leftDepth = depth(node.left)
      val rightDepth = depth(node.right)
      1 + math.max(leftDepth, rightDepth)
    } else 0
  }

  // p8
  def bin_search(t: Tree[Int], x: Int): Boolean = t match {
    // 재귀함수를 이용하여 이진 검색을 수행합니다.
    case Leaf(elem) => elem == x
    case Node(elem, left, right) =>
      if (x < elem) bin_search(left, x)
      else if (x > elem) bin_search(right, x)
      else true
  }

  // p9
  sealed trait Exp[T]
  case class ADD[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class SUB[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class MUL[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class DIV[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class MOD[T](left: Exp[T], right: Exp[T]) extends Exp[T]
  case class INT[T](value: T) extends Exp[T]

  def interp(exp: Exp[Int]): Int = exp match {
    // 재귀함수를 사용하여 함수를 작성하세요.
    case ADD(left, right) => interp(left) + interp(right)
    case SUB(left, right) => interp(left) - interp(right)
    case MUL(left, right) => interp(left) * interp(right)
    case DIV(left, right) => interp(left) / interp(right)
    case MOD(left, right) => interp(left) % interp(right)
    case INT(value) => value
  }

  // p10
  sealed trait Bool
  case class Neg(value: Bool) extends Bool
  case class Or(left: Bool, right: Bool) extends Bool
  case class And(left: Bool, right: Bool) extends Bool
  case class Imply(left: Bool, right: Bool) extends Bool
  case object True extends Bool
  case object False extends Bool

  def formula(fma: Bool): Boolean = fma match {
    // 재귀함수를 사용하여 Neg, Or, And, Imply 논리 연산자로 구성된 논리식을 계산하는 함수를 만드세요
    case Neg(value) => !formula(value)
    case Or(left, right) => formula(left) || formula(right)
    case And(left, right) => formula(left) && formula(right)
    case Imply(left, right) => !formula(left) || formula (right)
    case True => true
    case False => false
  }

  def main(args: Array[String]): Unit = {
    println("** p1 **")
    println(sum(100))
    println()

    println("** p2 **")
    println(fac(9))
    println()

    println("** p3 **")
    println(fib(10))
    println()

    println("** p4 **")
    println(gcd(15, 20))
    println()

    println("** p5 **")
    println(max(List(3, 4, 14, 7, 2)))
    println()

    println("** p6 **")
    println("sum of tree nodes: " + sum_tree(tree))
    println()

    println("** p7 **")
    println("depth of tree nodes: " + depth(tree))
    println()

    println("** p8 **")
    println("bin_search of tree nodes: " + bin_search(tree, 3))
    println()

    val exp = ADD(SUB(INT(100), INT(10)), MUL(INT(2), INT(8)))
    println("** p9 **")
    println("Interpretation of the expression is : " + interp(exp))
    println()

    val fma = Imply(And(True, Or(True, False)), False)
    println("** p10 **")
    println(formula(fma))
    println()
  }
}