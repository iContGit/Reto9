
import  java.util.Scanner;
object Reto9 {

  import Figura._                                   //Herencia del punto 5 y las clases a usar
  trait Figura
  case class Esfera (radio: Double) extends Figura
  case class Cubo (lado: Double) extends Figura


  def punto1 (x:Int):Boolean = {
    var i=1
    var esprimo = false
    var contador=0
    for (i<- 1 to (x))
      if ((x%i)==0 ) contador+=1
    if (contador==2) esprimo =true
    esprimo
  }

  def punto2(numero:Int ): Int = {
    if (numero == 0)  0
    else punto2(numero/10) + numero%10
  }

  def punto4a(numero:Int): Int={
    if (numero==0) 1
    else numero*punto4a(numero-1)
  }

  def punto4b(numero:Int):Int={
    var resultado=numero*numero*numero
    resultado
  }


  def punto4c(N: Int, f:(Int) => Int): Int = {
    if (N == 0) 0 else
      f(N) + punto4c(N-1,f)
  }

  object Figura {                               // Punto5
    def volumen(f: Figura): Double = f match {
      case Esfera(r) => (4*Math.PI*(Math.pow(r,3))/3)
      case Cubo(l) => Math.pow(l,3)
    }

  }

  def main(args: Array[String]): Unit = {
    val scanner = new Scanner(System.in)
    println("Ingrese numero para saber si es primo o no")
    val number = scanner.nextInt()
    println(punto1(number))
    println("Ingrese numero para el punto 2")
    val number2 = scanner.nextInt()
    println(punto2(number2))
    println("Ingrese numero para el punto 4a")
    val num4a = scanner.nextInt()
    println(punto4a(num4a))
    println("Ingrese numero para el punto 4b")
    val num4b = scanner.nextInt()
    println(punto4b(num4b))
    println("Ingrese numero para el punto 4c")
    val num4c = scanner.nextInt()
    println(punto4c(num4c,punto4b))
    println("Ingrese radio esfera:")
    val num5e = scanner.nextInt()
    val esfera: Figura = Esfera(num5e)
    println("Ingrese lado  cubo:")
    val num5c = scanner.nextInt()
    val cubo: Figura =  Cubo(num5c)
    println("Volumen esfera " + " "+ volumen(esfera) + " volumen cubo " + " " + volumen(cubo))
  }

}
