package hw01

object hw1 {
  // trait을 이용해 좋아하는 요일을 표현함
  sealed trait date
  case object MON extends date
  case object TUE extends date
  case object WED extends date
  case object THU extends date
  case object FRI extends date
  case object SAT extends date
  case object SUN extends date

  // 두 개의 실수를 나눈 값을 반환하는 클래스, 실수를 반환
  class Divider {
    def divide(n1: Double, n2: Double): Double = n1 / n2
  }

  val grade = Array("Aplus", "A", "Bplus", "B", "Cplus", "C", "Dplus", "D", "F") // 예상 학점, 문자열 배열
  val divi = new Divider // 두 개의 실수를 나눈 값을 반환하는 클래스, 실수

  var student_num = 201814267   // 학번, 정수
  var email = "liebenholz98@kangwon.ac.kr" // 이메일 주소, 문자열
  var phone_number = "01050996702"  // 폰 번호, 문자열
  var favorite_day = SAT  // 좋아하는 요일, trait 활용
  var expected_grade = grade(1)     // 예상 학점, 문자열 배열
  var is_male = true    // 성별, Boolean
  var result = divi.divide(10.0, 0.4) // (10.0 / 0.4) 를 계산한 결과, 실수

  // 결과를 출력하는 main 함수
  def main(args: Array[String]): Unit = {
    println("학번 : " + student_num)
    println("이메일 주소 : " + email)
    println("핸드폰 번호 : " + phone_number)
    println("좋아하는 요일 : " + favorite_day)
    println("예상 학점 : " + expected_grade)
    println("나는 남자이다 : " + is_male)
    println("10.0 / 0.4 : " + result)
  }
}
