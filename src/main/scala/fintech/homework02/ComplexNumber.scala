package fintech.homework02

class ComplexNumber(val real: Double, val image: Double) {

  def +(b: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real + b.real, image + b.image)
  }

  def *(b: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real * b.real - image * b.image, image * b.real + real * b.image)
  }

  private def GetModule(): Double = math.sqrt(real * real + image * image)

  private def GetArgument(): Double = math.atan(image / real)

  def ~(pow: Int): ComplexNumber = {
    val module = GetModule()
    val arg = GetArgument()
    val moduleInPow = math.pow(module, pow)
    new ComplexNumber(math.cos(pow * arg) * moduleInPow, math.sin(pow * arg) * moduleInPow)
  }

  override def toString: String = {
    if (image > 0) real.toString + " + " + image.toString + "i"
    else if (image < 0) real.toString + " - " + math.abs(image).toString + "i"
    else real.toString
  }

  override def equals(a: Any): Boolean = a.isInstanceOf[ComplexNumber] && a.asInstanceOf[ComplexNumber] == this

  override def hashCode(): Int = {
    val prime = 97
    var hash = 1
    hash = prime * hash + image.hashCode()
    hash = prime * hash + real.hashCode()
    hash
  }

  def ==(b: ComplexNumber): Boolean = real == b.real && image == b.image

  // Написать класс описывающий комплексные числа.
  // Реализовать проверку на равенство, умножение и сложение, toString.
  // Реализовать оператор возведения в целую степень: "~".
  // Реализовать тесты в ComplexNumberSpec
}
