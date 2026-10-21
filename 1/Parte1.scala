
object Parte1 {

  val duplicar: Int => Int = (x: Int) => x * 2
  val cuadrado: Int => Int = (x: Int) => x * x
  val esPar: Int => Boolean = (x: Int) => x % 2 == 0

  val cubo: Int => Int = x => x * x * x
  val promedio: (Double, Double) => Double = (a, b) => (a + b) / 2
  val mayor: (Int, Int) => Int = (a, b) => if (a > b) a else b
  val esMultiploDeTres: Int => Boolean = x => x % 3 == 0

  def transformar(numero: Int, operacion: Int => Int): Int =
    operacion(numero)

  def calcular(a: Double, b: Double, operacion: (Double, Double) => Double): Double =
    operacion(a, b)

  def crearMultiplicador(factor: Int): Int => Int =
    (x: Int) => x * factor

  val duplicarM: Int => Int = crearMultiplicador(2)
  val triplicarM: Int => Int = crearMultiplicador(3)
  val porDiez: Int => Int = crearMultiplicador(10)

  def aplicarDescuento(precio: Double, porcentaje: Double): Double =
    precio - (precio * porcentaje / 100)

  val descuento10: Double => Double = aplicarDescuento(_, 10)
  val descuento20: Double => Double = aplicarDescuento(_, 20)
  val descuento50: Double => Double = aplicarDescuento(_, 50)

  def elevar(exponente: Int)(base: Int): Int =
    if (exponente == 0) 1
    else base * elevar(exponente - 1)(base)

  val cuadradoE: Int => Int = elevar(2)
  val cuboE: Int => Int = elevar(3)
  val cuartaPotencia: Int => Int = elevar(4)


  val limpiar: String => String = s => s.trim
  val convertirMinusculas: String => String = s => s.toLowerCase
  val longitud: String => Int = s => s.length
  val esLongitudValida: Int => Boolean = n => n >= 5

  val validarTexto: String => Boolean =
    limpiar andThen convertirMinusculas andThen longitud andThen esLongitudValida


  def sumar(desde: Int, hasta: Int, transformar: Int => Int): Int =
    if (desde > hasta) 0
    else transformar(desde) + sumar(desde + 1, hasta, transformar)

  def producto(desde: Int, hasta: Int, transformar: Int => Int): Int =
    if (desde > hasta) 1
    else transformar(desde) * producto(desde + 1, hasta, transformar)


  def transformarTexto(texto: String, transformacion: Char => Char): String =
    if (texto.isEmpty) ""
    else transformacion(texto.head).toString + transformarTexto(texto.tail, transformacion)

  def buscarCaracter(texto: String, condicion: Char => Boolean): Boolean =
    if (texto.isEmpty) false
    else if (condicion(texto.head)) true
    else buscarCaracter(texto.tail, condicion)

  def main(args: Array[String]): Unit = {
    println("--- Ejercicio 1 ---")
    println(duplicar(5))            
    println(cuadrado(4))            
    println(esPar(8))               

    println("--- Ejercicio 2 ---")
    println(cubo(3))               
    println(promedio(4.0, 6.0))     
    println(mayor(3, 9))           
    println(esMultiploDeTres(9))    

    println("--- Ejercicio 3 ---")
    println(transformar(5, x => x * 2))   
    println(transformar(5, x => x * x))   
    println(transformar(5, x => x + 10))  
    println(transformar(5, x => -x))      

    println("--- Ejercicio 4 ---")
    println(calcular(10, 4, (a, b) => a + b))        
    println(calcular(10, 4, (a, b) => a - b))        
    println(calcular(10, 4, (a, b) => a * b))        
    println(calcular(10, 4, (a, b) => a / b))        
    println(calcular(10, 4, (a, b) => (a + b) / 2))  

    println("--- Ejercicio 5 ---")
    println(duplicarM(7))  
    println(triplicarM(7))  
    println(porDiez(7))     

    println("--- Ejercicio 6 ---")
    println(descuento10(500))  
    println(descuento20(500))  
    println(descuento50(500)) 

    println("--- Ejercicio 7 ---")
    println(cuadradoE(5))        
    println(cuboE(3))            
    println(cuartaPotencia(2))  

    println("--- Ejercicio 8 ---")
    println(validarTexto("  Hola  "))     
    println(validarTexto("  SCALA  "))   

    println("--- Ejercicio 9 ---")
    println(sumar(1, 4, x => x))         
    println(sumar(1, 3, x => x * x))   
    println(sumar(1, 3, x => x * x * x)) 
    println(producto(1, 4, x => x))      

    println("--- Ejercicio 10 ---")
    println(transformarTexto("scala", c => c.toUpper))        
    println(buscarCaracter("Scala2026", c => c.isDigit))      
    println(buscarCaracter("Scala", c => c == 'z'))           
  }
}
