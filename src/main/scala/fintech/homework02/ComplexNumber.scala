package fintech.homework02

class ComplexNumber(val real: Double, val image: Double) {

  def +(b: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real + b.real, image + b.image)
  }

  def *(b: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real * b.real - image * b.image, image * b.real + real * b.image)
  }

  private def GetModulus(): Double = math.sqrt(real * real + image * image)

  private def GetArgument(): Double = math.atan(image / real)

  def ~(pow: Int): ComplexNumber = {
    val module = GetModulus()
    val arg = GetArgument()
    val moduleInPow = math.pow(module, pow)
    new ComplexNumber(math.cos(pow * arg) * moduleInPow, math.sin(pow * arg) * moduleInPow)
  }

  private def imageToString():String=math.abs(image) match {
    case 1 => "i"
    case 0 => ""
    case _ => math.abs(image).toString + "i"
  }

  override def toString: String = {
    if (real == 0 && image == 0)
      return "0"
    real match {
      case 0 => (if (image < 0) "-") + imageToString()
      case _ =>
        val sign = if (image < 0) " - " else if (image > 0) " + " else ""
        real.toString + sign + imageToString()
    }
  }

  override def equals(a: Any): Boolean = {
    a match {
      case complex: ComplexNumber => real == complex.real && image == complex.image
      case _ => false
    }
  }

  override def hashCode(): Int = {
    val prime = 97
    var hash = 1
    hash = prime * hash + image.hashCode()
    hash = prime * hash + real.hashCode()
    hash
  }

  // Написать класс описывающий комплексные числа.
  // Реализовать проверку на равенство, умножение и сложение, toString.
  // Реализовать оператор возведения в целую степень: "~".
  // Реализовать тесты в ComplexNumberSpec
}
