package fintech.homework02

class ComplexNumber (val real: Double, val image: Double){

  val eps = 0.00000001

  def + (b: ComplexNumber): ComplexNumber={
    new ComplexNumber(real + b.real, image + b.image)
  }

  def * (b: ComplexNumber): ComplexNumber={
    new ComplexNumber(real * b.real - image * b.image, image * b.real + real * b.image)
  }

  private def GetModule(): Double=math.sqrt(real * real + image * image)

  private def GetArgument(): Double=math.atan(image / real)

  def ~ (pow: Int): ComplexNumber={
    val module = GetModule()
    val arg = GetArgument()
    val m = math.pow(module, pow)
    new ComplexNumber(math.round(math.cos(pow * arg) * m), math.round(math.sin(pow * arg) * m))
  }

  override def toString(): String= {
    if (image > 0) real.toString + " + " + image.toString + "i"
    else if (image < 0) real.toString + " - " + math.abs(image).toString + "i"
    else real.toString
  }

  def equals(b: ComplexNumber): Boolean = math.abs(real - b.real) < eps && math.abs(image - b.image) < eps

  def == (b: ComplexNumber): Boolean = equals(b)

  // Написать класс описывающий комплексные числа.
  // Реализовать проверку на равенство, умножение и сложение, toString.
  // Реализовать оператор возведения в целую степень: "~".
  // Реализовать тесты в ComplexNumberSpec
}
