package hw02

object hw2 {
  def sum(n: Int): Int = {
    var n_sum = 0
    // n이 0보다 작거나 같으면 출력값은 0이 됩니다.
    if (n>0) {
      // n이 0보다 크면 공식으로 계산합니다.
      n_sum = n * (n+1) / 2
    }
    return n_sum
  }

  def circle(r: Double): Double = {
    var cir_area = 0.0
    // r이 0.0보다 작거나 같으면 원의 넓이는 0이 됩니다.
    if (r>0) {
      // r이 0.0보다 크면 공식을 사용하여 계산합니다.
      cir_area = 3.14 * r * r
    }
    return cir_area
  }

  def concat(name: String): String = {
    // "Hello " 를 삽입합니다.
    var greetings = "Hello " + name
    return greetings
  }

  def xor(x: Boolean, y: Boolean): Boolean = {
    // 위의 표와 같은 조건을 사용하여
    // x,y의 입력에 따라 출력값을 정해줍니다.
    var b_xor = if (x == y) false else true
    return b_xor
  }

  def triangle(x: Int, y: Int, z: Int): Boolean = {
    // 세 변 중에 하나라도 0보다 작거나 같으면 삼각형은 존재하지 않습니다.
    var is_triangle = false
    if(x>0 && y>0 && z>0) {
      // 세 변이 모두 0보다 크면 가장 긴 변 c를 찾습니다.
      val c = List(x,y,z).max
      // 나머지 두 변의 합이 c보다 크면 삼각형이 존재합니다.
      if ((x+y+z-c) > c)
        is_triangle = true
    }
    return is_triangle
  }

  def int_if_then_else(b: Boolean, x: Int, y: Int): Int = {
    // b가 true이면 두 수의 합을
    return if(b) x+y
    // b가 false이면 두 수의 차를 구합니다.
    else x-y
  }

  def sum_of_fun_val(a: Int, b: Int, c: Int, d: Int, e: Int): Int = {
    // func(x) = ax^2 + b^x + c
    def func(x:Int): Int = {
      return a*x*x + b*x + c
    }
    // f(d) + f(e) 를 구해줍니다.
    return func(d) + func(e)
  }

  def comp3(a: Int, b: Int, c: Int, d: Int): Int = {
    // func(x) = ax^2 + b^x + c
    def func(x: Int): Int = {
      return a*x*x + b*x + c
    }
    // f(f(f(d)))를 구해줍니다.
    var result = d
    for (i<-1 to 3) {
      result = func(result)
    }
    return result
  }

  def string2(s: String): String = {
    // 입력한 문자열 s를 두 번 반복하는 값을 구해줍니다.
    var result = ""
    for (i<-1 to 2) {
      result = result + s
    }
    return result
  }

  def string256(s: String): StringBuilder = {
    // StringBuilder를 사용하여 구합니다.
    val sb = new StringBuilder()
    for(i<-1 to 256)
      sb.append(s)
    return sb
  }

  def string256_use_string2(s: String): String = {
    // string2를 호출하여 구합니다.
    var result = s
    for(i<-1 to 8)
      result = string2(result)
    return result
  }

  def main(args: Array[String]): Unit = {
    println("** p1 **")
    println(sum(-10))
    println(sum(100))
    println()

    println("** p2 **")
    println(circle(-10.1))
    println(circle(4.2))
    println()

    println("** p3 **")
    println(concat("Bob!"))
    println(concat("Alice!"))
    println()

    println("** p4 **")
    println(xor(true, true))
    println(xor(true, false))
    println(xor(false, true))
    println(xor(false, false))
    println()

    println("** p5 **")
    println(triangle(-3, 3, 1))
    println(triangle(3, 4, 5))
    println(triangle(100, 1, 2))
    println()

    println("** p6 **")
    println(int_if_then_else(true, 2, 100))
    println(int_if_then_else((100 < 2), 2, -2))
    println()

    println("** p7 **")
    println(sum_of_fun_val(1, 2, 1, 3, 4))
    println(sum_of_fun_val(1, -3, -1, 200, 123))
    println()

    println("** p8 **")
    println(comp3(1, 1, 1, 1))
    println(comp3(1, -2, 1, 3))
    println()

    println("** p9 **")
    println(string2("hi"))
    println(string2("abcde"))
    println()

    println("** p10 **")
    println("방법 1")
    println(string256("a"))
    println("방법 2")
    println(string256_use_string2("a"))
  }
}
