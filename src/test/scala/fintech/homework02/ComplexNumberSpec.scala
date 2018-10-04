package fintech.homework02
import org.scalatest.{FlatSpec, Matchers}

class ComplexNumberSpec extends FlatSpec with Matchers {

  "Sun of complex numbers " should "be equal to sum of real and sum of images" in {
    val a = new ComplexNumber(3, 8)
    val b = new ComplexNumber(2, 1)
    val c = a + b
    c.real should be (5)
    c.image should be (9)
  }

  "Composition of (a+bi)*(c+di)" should "be equal to (ac-bd)+(bc-ad)i" in {
    val a = new ComplexNumber(3, 4)
    val b = new ComplexNumber(2, 3)
    val c = a * b
    c.real should be (-6)
    c.image should be (17)
  }

  "ComplexNumber (a+bi) and (c+di)" should "be equal if a == c and b == i" in {
    val a = new ComplexNumber(3, 2)
    val b = new ComplexNumber(2, 3)
    val c = new ComplexNumber(2, 3)
    a == b should be (false)
    b == c should be (true)
  }

  "String of complex number (a+bi)" should "be a + bi" in {
    val a = new ComplexNumber(3, -2)
    val b = new ComplexNumber(2, 3)
    val c = new ComplexNumber(2, 0)
    a.toString() should be ("3.0 - 2.0i")
    b.toString() should be ("2.0 + 3.0i")
    c.toString() should be ("2.0")
  }

  "Complex number z in pow n" should "|z|^n*(cos(nf)+sin(nf)*i)" in {
    val a = new ComplexNumber(5, 2)
    val c = a ~ 2
    c should be (new ComplexNumber(21, 20))
  }
}
