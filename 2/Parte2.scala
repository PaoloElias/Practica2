
class Pelicula(
  val titulo: String,
  val duracionMinutos: Int,
  val clasificacion: String
)

object Pelicula {
  def apply(titulo: String, duracionMinutos: Int, clasificacion: String): Pelicula = {
    val duracionValida: Int = if (duracionMinutos <= 0) 90 else duracionMinutos
    new Pelicula(titulo, duracionValida, clasificacion)
  }
}

class FuncionCine(
  val pelicula: Pelicula,
  val sala: Int,
  val hora: String,
  val precioEntrada: Double,
  val asientosDisponibles: Int
) {

  def puedeReservar(cantidad: Int): Boolean =
    cantidad > 0 && cantidad <= asientosDisponibles

  def costoReserva(cantidad: Int): Double =
    cantidad * precioEntrada

  def reservar(cantidad: Int): FuncionCine =
    if (puedeReservar(cantidad))
      new FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles - cantidad)
    else
      new FuncionCine(pelicula, sala, hora, precioEntrada, asientosDisponibles)

  def descripcion: String =
    s"${pelicula.titulo}\n" +
      s"Sala: $sala\n" +
      s"Hora: $hora\n" +
      s"Precio: $precioEntrada Bs\n" +
      s"Asientos disponibles: $asientosDisponibles"
}

object FuncionCine {

  def apply(
    pelicula: Pelicula,
    sala: Int,
    hora: String,
    precioEntrada: Double,
    asientosDisponibles: Int
  ): FuncionCine = {
    val salaValida: Int = if (sala >= 1) sala else 1
    val precioValido: Double = if (precioEntrada >= 0) precioEntrada else 0.0
    val asientosValidos: Int = if (asientosDisponibles >= 0) asientosDisponibles else 0
    new FuncionCine(pelicula, salaValida, hora, precioValido, asientosValidos)
  }

  def funcionEstandar(pelicula: Pelicula, sala: Int, hora: String): FuncionCine =
    FuncionCine(pelicula, sala, hora, 30.0, 50)
}

object Parte2 {
  def main(args: Array[String]): Unit = {

    val pelicula1: Pelicula = Pelicula("Dune", 155, "PG-13")

    val funcion1: FuncionCine = FuncionCine(pelicula1, 3, "20:30", 35.0, 40)
    val funcion2: FuncionCine = FuncionCine.funcionEstandar(pelicula1, 1, "16:00")

    println(funcion1.descripcion)
    println(funcion1.puedeReservar(5))   
    println(funcion1.costoReserva(5))    

    val funcionReservada: FuncionCine = funcion1.reservar(5)

    println(funcion1.asientosDisponibles)     
    println(funcionReservada.asientosDisponibles) 

    println(funcion2.descripcion)
  }
}
