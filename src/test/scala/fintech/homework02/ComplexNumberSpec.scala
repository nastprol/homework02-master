package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {

  "Sun of complex numbers " should "be equal to sum of real and sum of images" in {
    val a = new ComplexNumber(3, 8)
    val b = new ComplexNumber(2, 1)
    val c = a + b
    val d = new ComplexNumber(1, 1)

    c should be (new ComplexNumber(5, 9))
    c should be (b + a)
    a + new ComplexNumber(0,0) should be (a)
    (a + b) + d should be (a + (b + d))
  }

  "Composition of (a+bi)*(c+di)" should "be equal to (ac-bd)+(bc-ad)i" in {
    val a = new ComplexNumber(3, 4)
    val b = new ComplexNumber(2, 3)
    val c = a * b
    val d = new ComplexNumber(2, 2)

    c should be (new ComplexNumber(-6, 17))
    c should be (b * a)
    (a * b) * d should be (a * (b * d))
    a * new ComplexNumber(1,0) should be (a)
    a * new ComplexNumber(0,0) should be (new ComplexNumber(0, 0))
    a * (b + d) should be (a * b + a * d)
    (a + b) * d should be (a * d + b * d)
  }

  "ComplexNumber (a+bi) and (c+di)" should "be equal if a == c and b == i" in {
    val a = new ComplexNumber(3, 2)
    val b = new ComplexNumber(2, 3)
    val c = new ComplexNumber(2, 3)

    a == b should be (false)
    b == a should be (false)
    b == c should be (true)
    c == b should be (true)
    a == null should be (false)
    a == a should be (true)
  }

  "String of complex number (a+bi)" should "be a + bi" in {
    val a = new ComplexNumber(3, -2)
    val b = new ComplexNumber(2, 3)
    val c = new ComplexNumber(-2, 0)
    val d = new ComplexNumber(0, -2)
    val e = new ComplexNumber(0, 0)

    a.toString() should be ("3.0 - 2.0i")
    b.toString() should be ("2.0 + 3.0i")
    c.toString() should be ("-2.0")
    d.toString() should be ("-2.0i")
    e.toString() should be ("0")
  }

  "Complex number z in pow n" should "|z|^n*(cos(nf)+sin(nf)*i)" in {
    val a = new ComplexNumber(5, 2)
    val c = a ~ 2
    val eps = 0.00000001

    math.abs(c.real - 21) < eps should be (true)
    math.abs(c.image - 20) < eps should be (true)
  }
}
